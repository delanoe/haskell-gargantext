{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.AsyncTask where

import Control.Concurrent.Async (poll)
import Control.Concurrent.MVar (readMVar)
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (ask, liftIO)
import Data.Either (Either(..))
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes)
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  , lift
  )
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.API.Prelude (GargM, GargError, HasJobEnv')
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Servant.Job.Async (HasJobEnv(job_env), jenv_jobs, job_async)
import Servant.Job.Core (env_item, env_map, env_state_mvar)

data JobLogArgs
  = JobLogArgs
    { job_log_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => JobLogArgs -> GqlM e env (Map Int JobLog)
resolveJobLogs JobLogArgs { job_log_id } = dbJobLogs job_log_id

dbJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => Int -> GqlM e env (Map Int JobLog)
dbJobLogs _job_log_id = do
  --getJobLogs job_log_id
  lift $ do
    env <- ask
    --val <- liftBase $ readMVar $ env ^. job_env . jenv_jobs . env_state_mvar
    var <- liftIO $ readMVar (env ^. job_env . jenv_jobs . env_state_mvar)
    let envItems = var ^. env_map
    -- printDebug "[dbJobLogs] env ^. job_env ^. jenv_jobs" $ length $ IntMap.keys envItems
    -- printDebug "[dbJobLogs] job_log_id" job_log_id
    --pure $ IntMap.elems val
    liftIO $ do
      let jobsList = IntMap.toList $ IntMap.map (\e -> e ^. env_item . job_async) envItems
      results <- mapM (\(k, v) -> do
                          p <- poll v
                          let kv = case p of
                                Nothing -> Nothing
                                Just p' -> case p' of
                                  Left _ -> Nothing
                                  Right p'' -> Just (k, p'')
                          pure kv) jobsList
      pure $ Map.fromList $ catMaybes results
