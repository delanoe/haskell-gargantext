-- |

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Gargantext.API.Admin.EnvTypes where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Pool (Pool)
import Data.Sequence (Seq)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)
import Servant.Job.Async (HasJobEnv(..), Job)
import System.Log.FastLogger
import qualified Servant.Job.Core

import Gargantext.API.Admin.Types
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Prelude (GargError)
import Gargantext.Core.NodeStory
import Gargantext.Core.Mail.Types (HasMail, mailSettings)
import Gargantext.Core.NLP (NLPServerMap, HasNLPServer(..))
import Gargantext.Database.Prelude (HasConnectionPool(..), HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..))
import Gargantext.Prelude.Mail.Types (MailConfig)

import qualified Gargantext.Utils.Jobs.Monad as Jobs

data GargJob
  = TableNgramsJob
  | ForgotPasswordJob
  | UpdateNgramsListJobJSON
  | UpdateNgramsListJobCSV
  | AddContactJob
  | AddFileJob
  | DocumentFromWriteNodeJob
  | UpdateNodeJob
  | UploadFrameCalcJob
  | UploadDocumentJob
  | NewNodeJob
  | AddCorpusQueryJob
  | AddCorpusFormJob
  | AddCorpusFileJob
  | AddAnnuaireFormJob
  | RecomputeGraphJob
  deriving (Show, Eq, Ord, Enum, Bounded)

data Env = Env
  { _env_settings  :: !Settings
  , _env_logger    :: !LoggerSet
  , _env_pool      :: !(Pool Connection)
  , _env_nodeStory :: !NodeStoryEnv
  , _env_manager   :: !Manager
  , _env_self_url  :: !BaseUrl
  , _env_scrapers  :: !ScrapersEnv
  , _env_jobs      :: !(Jobs.JobEnv GargJob (Seq JobLog) JobLog)
  , _env_config    :: !GargConfig
  , _env_mail      :: !MailConfig
  , _env_nlp       :: !NLPServerMap
  }
  deriving (Generic)

makeLenses ''Env

instance HasConfig Env where
  hasConfig = env_config

instance HasConnectionPool Env where
  connPool = env_pool

instance HasNodeStoryEnv Env where
  hasNodeStory = env_nodeStory

instance HasNodeStoryVar Env where
  hasNodeStoryVar = hasNodeStory . nse_getter

instance HasNodeStorySaver Env where
  hasNodeStorySaver = hasNodeStory . nse_saver

instance HasNodeStoryImmediateSaver Env where
  hasNodeStoryImmediateSaver = hasNodeStory . nse_saver_immediate

instance HasNodeArchiveStoryImmediateSaver Env where
  hasNodeArchiveStoryImmediateSaver = hasNodeStory . nse_archive_saver_immediate

instance HasSettings Env where
  settings = env_settings

instance HasMail Env where
  mailSettings = env_mail

instance HasNLPServer Env where
  nlpServer = env_nlp

instance Servant.Job.Core.HasEnv Env (Job JobLog JobLog) where
  _env = env_scrapers . Servant.Job.Core._env

instance HasJobEnv Env JobLog JobLog where
  job_env = env_scrapers

instance Jobs.MonadJob (ReaderT Env (ExceptT GargError IO)) GargJob (Seq JobLog) JobLog where
  getJobEnv = asks (view env_jobs)

instance Jobs.MonadJobStatus (ReaderT Env (ExceptT GargError IO)) where
  type JobType        (ReaderT Env (ExceptT GargError IO)) = GargJob
  type JobOutputType  (ReaderT Env (ExceptT GargError IO)) = JobLog
  type JobEventType   (ReaderT Env (ExceptT GargError IO)) = JobLog
  type JobErrorType   (ReaderT Env (ExceptT GargError IO)) = GargError

data MockEnv = MockEnv
  { _menv_firewall :: !FireWall
  }
  deriving (Generic)

makeLenses ''MockEnv



data DevEnv = DevEnv
  { _dev_env_settings  :: !Settings
  , _dev_env_config    :: !GargConfig
  , _dev_env_pool      :: !(Pool Connection)
  , _dev_env_nodeStory :: !NodeStoryEnv
  , _dev_env_mail      :: !MailConfig
  , _dev_env_nlp       :: !NLPServerMap
  }

makeLenses ''DevEnv

instance HasConfig DevEnv where
  hasConfig = dev_env_config

instance HasConnectionPool DevEnv where
  connPool = dev_env_pool

instance HasSettings DevEnv where
  settings = dev_env_settings


instance HasNodeStoryEnv DevEnv where
  hasNodeStory = dev_env_nodeStory

instance HasNodeStoryVar DevEnv where
  hasNodeStoryVar = hasNodeStory . nse_getter

instance HasNodeStorySaver DevEnv where
  hasNodeStorySaver = hasNodeStory . nse_saver

instance HasNodeStoryImmediateSaver DevEnv where
  hasNodeStoryImmediateSaver = hasNodeStory . nse_saver_immediate

instance HasNodeArchiveStoryImmediateSaver DevEnv where
  hasNodeArchiveStoryImmediateSaver = hasNodeStory . nse_archive_saver_immediate

instance HasMail DevEnv where
  mailSettings = dev_env_mail

instance HasNLPServer DevEnv where
  nlpServer = dev_env_nlp
