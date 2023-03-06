{-# LANGUAGE GADTs #-}
module Gargantext.Utils.Jobs.Map where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Map.Strict (Map)
import Data.Time.Clock
import Prelude

import qualified Data.Map.Strict as Map

import Gargantext.Utils.Jobs.Settings

-- | (Mutable) 'Map' containing job id -> job info mapping.
newtype JobMap jid w a = JobMap
  { jobMap :: TVar (Map jid (JobEntry jid w a))
  }

-- | Information associated to a job ID
data JobEntry jid w a = JobEntry
  { jID           :: jid
  , jTask         :: J w a
  , jTimeoutAfter :: Maybe UTCTime
  , jRegistered   :: UTCTime
  , jStarted      :: Maybe UTCTime
  , jEnded        :: Maybe UTCTime
  }

-- | A job computation, which has a different representation depending on the
--   status of the job.
--
--   A queued job consists of the input to the computation and the computation.
--   A running job consists of an 'Async' as well as an action to get the current logs.
--   A done job consists of the result of the computation and the final logs.
data J w a
  = QueuedJ (QueuedJob w a)
  | RunningJ (RunningJob w a)
  | DoneJ w (Either SomeException a)

-- | An unexecuted job is an input paired with a computation
--   to run with it. Input type is "hidden" to
--   be able to store different job types together.
data QueuedJob w r where
  QueuedJob :: a -> (a -> Logger w -> IO r) -> QueuedJob w r

-- | A running job points to the async computation for the job and provides a
--   function to peek at the current logs.
data RunningJob w a = RunningJob
  { rjAsync  :: Async a
  , rjGetLog :: IO w
  }

-- | A @'Logger' w@ is a function that can do something with "messages" of type
--   @w@ in IO.
type Logger w = w -> IO ()

newJobMap :: IO (JobMap jid w a)
newJobMap = JobMap <$> newTVarIO Map.empty

-- | Lookup a job by ID
lookupJob
  :: Ord jid
  => jid
  -> JobMap jid w a
  -> IO (Maybe (JobEntry jid w a))
lookupJob jid (JobMap mvar) = Map.lookup jid <$> readTVarIO mvar

-- | Ready to use GC thread
gcThread :: Ord jid => JobSettings -> JobMap jid w a -> IO ()
gcThread js (JobMap mvar) = go
  where go = do
          now <- getCurrentTime
          candidateEntries <- Map.filter (expired now) <$> readTVarIO mvar
          forM_ candidateEntries $ \je -> do
            mrunningjob <- atomically $ do
              case jTask je of
                RunningJ rj -> modifyTVar' mvar (Map.delete (jID je))
                            >> return (Just rj)
                _ -> return Nothing
            case mrunningjob of
              Nothing -> return ()
              Just a  -> killJ a
          threadDelay (jsGcPeriod js * 1000000)
          go

        expired now jobentry = case jTimeoutAfter jobentry of
          Just t -> now >= t
          _      -> False

-- | Make a 'Logger' that 'mappend's monoidal values in a 'TVar'.
jobLog :: Semigroup w => TVar w -> Logger w -- w -> IO ()
jobLog logvar = \w -> atomically $ modifyTVar' logvar (\old_w -> old_w <> w)

-- | Generating new 'JobEntry's.
addJobEntry
  :: Ord jid
  => jid
  -> a
  -> (a -> Logger w -> IO r)
  -> JobMap jid w r
  -> IO (JobEntry jid w r)
addJobEntry jid input f (JobMap mvar) = do
  now <- getCurrentTime
  let je = JobEntry
        { jID = jid
        , jTask = QueuedJ (QueuedJob input f)
        , jRegistered = now
        , jTimeoutAfter = Nothing
        , jStarted = Nothing
        , jEnded = Nothing
        }
  atomically $ modifyTVar' mvar (Map.insert jid je)
  return je

deleteJob :: Ord jid => jid -> JobMap jid w a -> STM ()
deleteJob jid (JobMap mvar) = modifyTVar' mvar (Map.delete jid)

runJob
  :: (Ord jid, Monoid w)
  => jid
  -> QueuedJob w a
  -> JobMap jid w a
  -> JobSettings
  -> IO (RunningJob w a)
runJob jid qj (JobMap mvar) js = do
  rj <- runJ qj
  now <- getCurrentTime
  atomically $ modifyTVar' mvar $
    flip Map.adjust jid $ \je ->
     je { jTask = RunningJ rj
        , jStarted = Just now
        , jTimeoutAfter = Just $ addUTCTime (fromIntegral (jsJobTimeout js)) now
        }
  return rj

waitJobDone
  :: Ord jid
  => jid
  -> RunningJob w a
  -> JobMap jid w a
  -> IO (Either SomeException a, w)
waitJobDone jid rj (JobMap mvar) = do
  r <- waitJ rj
  now <- getCurrentTime
  logs <- rjGetLog rj
  atomically $ modifyTVar' mvar $
    flip Map.adjust jid $ \je ->
      je { jEnded = Just now, jTask = DoneJ logs r }
  return (r, logs)

-- | Turn a queued job into a running job by setting up the logging of @w@s and
--   firing up the async action.
runJ :: Monoid w => QueuedJob w a -> IO (RunningJob w a)
runJ (QueuedJob a f) = do
  logs <- newTVarIO mempty
  act <- async $ f a (jobLog logs)
  let readLogs = readTVarIO logs
  return (RunningJob act readLogs)

-- | Wait for a running job to return (blocking).
waitJ :: RunningJob w a -> IO (Either SomeException a)
waitJ (RunningJob act _) = waitCatch act

-- | Poll a running job to see if it's done.
pollJ :: RunningJob w a -> IO (Maybe (Either SomeException a))
pollJ (RunningJob act _) = poll act

-- | Kill a running job by cancelling the action.
killJ :: RunningJob w a -> IO  ()
killJ (RunningJob act _) = cancel act
