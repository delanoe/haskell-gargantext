{-# LANGUAGE MultiWayIf, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}
module Gargantext.Utils.Jobs.Monad (
  -- * Types and classes
    JobEnv(..)
  , NumRunners
  , JobError(..)

  , MonadJob(..)
  , MonadJobStatus(..)

  -- * Functions
  , newJobEnv
  , defaultJobSettings
  , genSecret
  , getJobsSettings
  , getJobsState
  , getJobsMap
  , getJobsQueue
  , queueJob
  , findJob
  , checkJID
  , withJob
  , handleIDError
  , removeJob
  ) where

import Gargantext.Utils.Jobs.Settings
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Queue
import Gargantext.Utils.Jobs.State

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Except
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Time.Clock
import Network.HTTP.Client (Manager)
import Prelude

import qualified Servant.Job.Core as SJ
import qualified Servant.Job.Types as SJ

data JobEnv t w a = JobEnv
  { jeSettings :: JobSettings
  , jeState    :: JobsState t w a
  , jeManager  :: Manager
  }

newJobEnv
  :: (EnumBounded t, Monoid w)
  => JobSettings
  -> Map t Prio
  -> Manager
  -> IO (JobEnv t w a)
newJobEnv js prios mgr = JobEnv js <$> newJobsState js prios <*> pure mgr

type NumRunners = Int

defaultJobSettings :: NumRunners -> SJ.SecretKey -> JobSettings
defaultJobSettings numRunners k = JobSettings
  { jsNumRunners = numRunners
  , jsJobTimeout = 30 * 60 -- 30 minutes
  , jsIDTimeout  = 30 * 60 -- 30 minutes
  , jsGcPeriod   =  1 * 60 -- 1 minute
  , jsSecretKey  = k
  }

genSecret :: IO SJ.SecretKey
genSecret = SJ.generateSecretKey

class MonadIO m => MonadJob m t w a | m -> t w a where
  getJobEnv :: m (JobEnv t w a)

getJobsSettings :: MonadJob m t w a => m JobSettings
getJobsSettings = jeSettings <$> getJobEnv

getJobsState :: MonadJob m t w a => m (JobsState t w a)
getJobsState = jeState <$> getJobEnv

getJobsMap :: MonadJob m t w a => m (JobMap (SJ.JobID 'SJ.Safe) w a)
getJobsMap = jobsData <$> getJobsState

getJobsQueue :: MonadJob m t w a => m (Queue t (SJ.JobID 'SJ.Safe))
getJobsQueue = jobsQ <$> getJobsState

queueJob
  :: (MonadJob m t w a, Ord t)
  => t
  -> i
  -> (SJ.JobID 'SJ.Safe -> i -> Logger w -> IO a)
  -> m (SJ.JobID 'SJ.Safe)
queueJob jobkind input f = do
  js <- getJobsSettings
  st <- getJobsState
  liftIO (pushJob jobkind input f js st)

findJob
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Safe
  -> m (Maybe (JobEntry (SJ.JobID 'SJ.Safe) w a))
findJob jid = do
  jmap <- getJobsMap
  liftIO $ lookupJob jid jmap

data JobError
  = InvalidIDType
  | IDExpired
  | InvalidMacID
  | UnknownJob
  | JobException SomeException
  deriving Show

checkJID
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Unsafe
  -> m (Either JobError (SJ.JobID 'SJ.Safe))
checkJID (SJ.PrivateID tn n t d) = do
  now <- liftIO getCurrentTime
  js <- getJobsSettings
  if | tn /= "job" -> return (Left InvalidIDType)
     | now > addUTCTime (fromIntegral $ jsIDTimeout js) t -> return (Left IDExpired)
     | d /= SJ.macID tn (jsSecretKey js) t n -> return (Left InvalidMacID)
     | otherwise -> return $ Right (SJ.PrivateID tn n t d)

withJob
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Unsafe
  -> (SJ.JobID 'SJ.Safe -> JobEntry (SJ.JobID 'SJ.Safe) w a -> m r)
  -> m (Either JobError (Maybe r))
withJob jid f = do
  r <- checkJID jid
  case r of
    Left e -> return (Left e)
    Right jid' -> do
      mj <- findJob jid'
      case mj of
        Nothing -> return (Right Nothing)
        Just j  -> Right . Just <$> f jid' j

handleIDError
  :: MonadError e m
  => (JobError -> e)
  -> m (Either JobError a)
  -> m a
handleIDError toE act = act >>= \r -> case r of
  Left err -> throwError (toE err)
  Right a  -> return a

removeJob
  :: (Ord t, MonadJob m t w a)
  => Bool -- is it queued (and we have to remove jid from queue)
  -> t
  -> SJ.JobID 'SJ.Safe
  -> m ()
removeJob queued t jid = do
  q <- getJobsQueue
  m <- getJobsMap
  liftIO . atomically $ do
    when queued $
      deleteQueue t jid q
    deleteJob jid m

--
-- Tracking jobs status
--

-- | A monad to query for the status of a particular job /and/ submit updates for in-progress jobs.
class MonadJob m (JobType m) (Seq (JobEventType m)) (JobOutputType m) => MonadJobStatus m where
  type JobType        m :: Type
  type JobOutputType  m :: Type
  type JobEventType   m :: Type
  type JobErrorType   m :: Type
