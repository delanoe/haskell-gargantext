{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.AsyncTask where

import Control.Lens
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
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Prelude
import GHC.Generics (Generic)

data JobLogArgs
  = JobLogArgs
    { job_log_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveJobLogs
  :: (HasConnectionPool env, HasConfig env)
  => JobLogArgs -> GqlM e env [JobLog]
resolveJobLogs JobLogArgs { job_log_id } = dbJobLogs job_log_id

dbJobLogs
  :: (HasConnectionPool env, HasConfig env)
  => Int -> GqlM e env [JobLog]
dbJobLogs job_log_id = do
  getJobLogs job_log_id
