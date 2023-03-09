module Gargantext.Utils.Jobs.State where

import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Queue
import Gargantext.Utils.Jobs.Settings

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Time.Clock
import Prelude

import qualified Data.Map.Strict as Map
import qualified Servant.Job.Core as SJ
import qualified Servant.Job.Types as SJ

type IDGenerator = TVar Int

data JobsState t w a = JobsState
  { jobsData  :: JobMap (SJ.JobID 'SJ.Safe) w a
  , jobsQ     :: Queue t (SJ.JobID 'SJ.Safe)
  , jobsIdGen :: IDGenerator
  , jsGC      :: Async ()
  , jsRunners :: [Async ()]
  }

nextID :: JobSettings -> JobsState t w a -> IO (SJ.JobID 'SJ.Safe)
nextID js st = do
  now <- getCurrentTime
  n <- atomically $ stateTVar (jobsIdGen st) $ \i -> (i, i+1)
  return $ SJ.newID (Proxy :: Proxy "job") (jsSecretKey js) now n

newJobsState
  :: forall t w a.
     (EnumBounded t, Monoid w)
  => JobSettings
  -> Map t Prio
  -> IO (JobsState t w a)
newJobsState js prios = do
  jmap <- newJobMap
  idgen <- newTVarIO 0
  (q, runners) <- newQueueWithRunners (jsNumRunners js) prios (picker jmap) $ \jid -> do
    mje <- lookupJob jid jmap
    case mje of
      Nothing -> return ()
      Just je -> case jTask je of
        QueuedJ qj -> do
          rj <- runJob jid qj jmap js
          (_res, _logs) <- waitJobDone jid rj jmap
          return ()
        _ -> return ()
  putStrLn $ "Starting " ++ show (jsNumRunners js) ++ " job runners."
  gcAsync <- async $ gcThread js jmap
  runnersAsyncs <- traverse async runners
  return (JobsState jmap q idgen gcAsync runnersAsyncs)

  where picker
          :: JobMap (SJ.JobID 'SJ.Safe) w a
          -> Picker (SJ.JobID 'SJ.Safe)
        picker (JobMap jmap) xs = do
          jinfos <- fmap catMaybes . forM xs $ \(jid, popjid) -> do
            mje <- Map.lookup jid <$> readTVar jmap
            case mje of
              Nothing -> return Nothing
              Just je -> return $ Just (jid, popjid, jRegistered je)
          let (jid, popjid, _) = minimumBy (comparing _3) jinfos
          return (jid, popjid)

        _3 (_, _, c) = c
pushJob
  :: Ord t
  => t
  -> a
  -> (a -> Logger w -> IO r)
  -> JobSettings
  -> JobsState t w r
  -> IO (SJ.JobID 'SJ.Safe)
pushJob jobkind input f js st@(JobsState jmap jqueue _idgen _ _) = do
  jid <- nextID js st
  _je <- addJobEntry jid input f jmap
  addQueue jobkind jid jqueue
  return jid
