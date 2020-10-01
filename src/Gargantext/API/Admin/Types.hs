-- |

{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Types where

import Control.Lens
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Enum
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant.Auth.Server (JWTSettings, CookieSettings(..))
import Servant.Client (BaseUrl)
import Servant.Job.Async (HasJobEnv(..), Job)
import System.Log.FastLogger
import qualified Servant.Job.Core

import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams.Types (HasRepoVar(..), HasRepoSaver(..), HasRepo(..), RepoEnv(..))
import Gargantext.Database.Prelude (HasConnectionPool(..), HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..))

type PortNumber = Int

data SendEmailType = SendEmailViaAws
                   | LogEmailToConsole
                   | WriteEmailToFile
    deriving (Show, Read, Enum, Bounded, Generic)


data Settings = Settings
    { _allowedOrigin   :: ByteString   -- allowed origin for CORS
    , _allowedHost     :: ByteString   -- allowed host for CORS
    , _appPort         :: PortNumber
    , _logLevelLimit   :: LogLevel -- log level from the monad-logger package
--    , _dbServer        :: Text
--    ^ this is not used yet
    , _jwtSettings     :: JWTSettings
    , _cookieSettings  :: CookieSettings
    , _sendLoginEmails :: SendEmailType
    , _scrapydUrl      :: BaseUrl
    , _config          :: GargConfig
    }

makeLenses ''Settings

class HasSettings env where
  settings :: Getter env Settings


data FireWall = FireWall { unFireWall :: Bool }

data Env = Env
  { _env_settings :: !Settings
  , _env_logger   :: !LoggerSet
  , _env_pool     :: !(Pool Connection)
  , _env_repo     :: !RepoEnv
  , _env_manager  :: !Manager
  , _env_self_url :: !BaseUrl
  , _env_scrapers :: !ScrapersEnv
  , _env_gargConfig :: !GargConfig
  }
  deriving (Generic)

makeLenses ''Env

instance HasConfig Env where
  hasConfig = env_gargConfig

instance HasConnectionPool Env where
  connPool = env_pool

instance HasRepoVar Env where
  repoVar = repoEnv . repoVar

instance HasRepoSaver Env where
  repoSaver = repoEnv . repoSaver

instance HasRepo Env where
  repoEnv = env_repo

instance HasSettings Env where
  settings = env_settings

instance Servant.Job.Core.HasEnv Env (Job JobLog JobLog) where
  _env = env_scrapers . Servant.Job.Core._env

instance HasJobEnv Env JobLog JobLog where
  job_env = env_scrapers

data MockEnv = MockEnv
  { _menv_firewall :: !FireWall
  }
  deriving (Generic)

makeLenses ''MockEnv


data DevEnv = DevEnv
  { _dev_env_pool     :: !(Pool Connection)
  , _dev_env_repo     :: !RepoEnv
  , _dev_env_settings :: !Settings
  , _dev_env_config   :: !GargConfig
  }

makeLenses ''DevEnv

instance HasConfig DevEnv where
  hasConfig = dev_env_config

instance HasConnectionPool DevEnv where
  connPool = dev_env_pool

instance HasRepoVar DevEnv where
  repoVar = repoEnv . repoVar

instance HasRepoSaver DevEnv where
  repoSaver = repoEnv . repoSaver

instance HasRepo DevEnv where
  repoEnv = dev_env_repo

instance HasSettings DevEnv where
  settings = dev_env_settings
