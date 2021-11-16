{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.AsyncTask where

import Control.Concurrent.MVar (readMVar)
import Control.Lens
import Control.Monad.Base (liftBase)
import Control.Monad.Reader (ask, liftIO)
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , ResolverM
  , QUERY
  , lift
  )
import Data.Text (Text)
import qualified Data.Text as T
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.API.Prelude (GargM, GargError, HasJobEnv')
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Servant.Job.Async (HasJobEnv(job_env), jenv_jobs)
import Servant.Job.Core (env_map, env_state_mvar)

data JobLogArgs
  = JobLogArgs
    { job_log_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => JobLogArgs -> GqlM e env [JobLog]
resolveJobLogs JobLogArgs { job_log_id } = dbJobLogs job_log_id

dbJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => Int -> GqlM e env [JobLog]
dbJobLogs job_log_id = do
  --getJobLogs job_log_id
  env <- ask
  _ <- lift $ do
    --val <- liftBase $ readMVar $ env ^. job_env . jenv_jobs . env_state_mvar
    let val = env
    printDebug "[dbJobLogs] env ^. job_env ^. jenv_jobs" val 
    printDebug "[dbJobLogs] job_log_id" job_log_id
  pure []
