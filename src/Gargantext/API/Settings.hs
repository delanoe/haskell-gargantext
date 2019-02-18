{-| 
Module      : Gargantext.API.Settings
Description : Settings of the API (Server and Client)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE RankNTypes                  #-}

module Gargantext.API.Settings
    where

import System.Directory
import System.Log.FastLogger
import GHC.Enum
import GHC.Generics (Generic)
import Prelude (Bounded(), fail)
import System.Environment (lookupEnv)
import System.IO (FilePath)
import Database.PostgreSQL.Simple (Connection, connect)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Either (either)
import Data.JsonState (mkSaveState)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Units
import Data.ByteString.Lazy.Internal

import Servant
import Servant.Client (BaseUrl, parseBaseUrl)
import Servant.Job.Async (newJobEnv, defaultSettings)
import Web.HttpApiData (parseUrlPiece)
import qualified Jose.Jwk as Jose
import qualified Jose.Jwa as Jose

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Lens
import Gargantext.Prelude
import Gargantext.Database.Utils (databaseParameters, HasConnection(..), Cmd', runCmd)
import Gargantext.API.Ngrams (NgramsRepo, HasRepoVar(..), HasRepoSaver(..), initMockRepo, r_version, saveRepo)
import Gargantext.API.Orchestrator.Types

type PortNumber = Int

data SendEmailType = SendEmailViaAws
                   | LogEmailToConsole
                   | WriteEmailToFile
    deriving (Show, Read, Enum, Bounded, Generic)


data Settings = Settings
    { _allowedOrigin   :: ByteString -- ^ allowed origin for CORS
    , _allowedHost     :: ByteString   -- ^ allowed host for CORS
    , _appPort         :: PortNumber
    , _logLevelLimit   :: LogLevel -- ^ log level from the monad-logger package
--    , _dbServer        :: Text
--    ^ this is not used yet
    , _jwtSecret       :: Jose.Jwk -- ^ key from the jose-jwt package
    , _sendLoginEmails :: SendEmailType
    , _scrapydUrl      :: BaseUrl
    }

makeLenses ''Settings


parseJwk :: Text -> Jose.Jwk
parseJwk secretStr = jwk
    where
        secretBs = encodeUtf8 secretStr
        jwk      = Jose.SymmetricJwk secretBs 
                                     Nothing 
                                     Nothing 
                                     (Just $ Jose.Signed Jose.HS256)

devSettings :: Settings
devSettings = Settings
    { _allowedOrigin = "http://localhost:8008"
    , _allowedHost = "localhost:3000"
    , _appPort = 3000
    , _logLevelLimit = LevelDebug
--    , _dbServer = "localhost"
    -- generate with dd if=/dev/urandom bs=1 count=32 | base64
    -- make sure jwtSecret differs between development and production, because you do not want
    -- your production key inside source control.
    , _jwtSecret = parseJwk "MVg0YAPVSPiYQc/qIs/rV/X32EFR0zOJWfHFgMbszMw="
    , _sendLoginEmails = LogEmailToConsole
    , _scrapydUrl = fromMaybe (panic "Invalid scrapy URL") $ parseBaseUrl "http://localhost:6800"
    }



reqSetting :: FromHttpApiData a => Text -> IO a
reqSetting name = do
    e <- fromMaybe (panic $ "Missing " <> name) <$> lookupEnv (unpack name)
    pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

optSetting :: FromHttpApiData a => Text -> a -> IO a
optSetting name d = do
    me <- lookupEnv (unpack name)
    case me of
        Nothing -> pure d
        Just e -> pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

--settingsFromEnvironment :: IO Settings
--settingsFromEnvironment =
--    Settings <$> (encodeUtf8 <$> reqSetting "ALLOWED_ORIGIN")
--             <*> (encodeUtf8 <$> reqSetting "ALLOWED_HOST")
--             <*> optSetting "PORT" 3000
--             <*> (parseLogLevel <$> optSetting "LOG_LEVEL" "warn")
--             <*> reqSetting "DB_SERVER"
--             <*> (parseJwk <$> reqSetting "JWT_SECRET")
--             <*> optSetting "SEND_EMAIL" SendEmailViaAws

data FireWall = FireWall { unFireWall :: Bool }

data Env = Env
  { _env_settings   :: !Settings
  , _env_logger     :: !LoggerSet
  , _env_conn       :: !Connection
  , _env_repo_var   :: !(MVar NgramsRepo)
  , _env_repo_saver :: !(IO ())
  , _env_manager    :: !Manager
  , _env_self_url   :: !BaseUrl
  , _env_scrapers   :: !ScrapersEnv
  }
  deriving (Generic)

makeLenses ''Env

instance HasConnection Env where
  connection = env_conn

instance HasRepoVar Env where
  repoVar = env_repo_var

instance HasRepoSaver Env where
  repoSaver = env_repo_saver

data MockEnv = MockEnv
  { _menv_firewall :: !FireWall
  }
  deriving (Generic)

makeLenses ''MockEnv

repoSnapshot :: FilePath
repoSnapshot = "repo.json"

readRepo :: IO (MVar NgramsRepo)
readRepo = do
  -- | Does file exist ? :: Bool
  repoFile <- doesFileExist repoSnapshot

  -- | Is file not empty ? :: Bool
  repoExists <- if repoFile
             then (>0) <$> getFileSize repoSnapshot
             else pure False

  newMVar =<<
    if repoExists
      then do
        e_repo <- eitherDecodeFileStrict repoSnapshot
        repo   <- either fail pure e_repo
        let archive = repoSnapshot <> ".v" <> show (repo ^. r_version)
        copyFile repoSnapshot archive
        pure repo
      else
        pure mempty

mkRepoSaver :: MVar NgramsRepo -> IO (IO ())
mkRepoSaver repo_var = do
  saveAction <- mkSaveState (10 :: Second) repoSnapshot
  pure $ readMVar repo_var >>= saveAction

newEnv :: PortNumber -> FilePath -> IO Env
newEnv port file = do
  manager <- newTlsManager
  settings <- pure (devSettings & appPort .~ port) -- TODO read from 'file'
  when (port /= settings ^. appPort) $
    panic "TODO: conflicting settings of port"
  
  self_url <- parseBaseUrl $ "http://0.0.0.0:" <> show port
  param    <- databaseParameters file
  conn     <- connect param
  
  repo_var     <- readRepo
  repo_saver   <- mkRepoSaver repo_var
  scrapers_env <- newJobEnv defaultSettings manager
  logger <- newStderrLoggerSet defaultBufSize
  
  pure $ Env
    { _env_settings   = settings
    , _env_logger     = logger
    , _env_conn       = conn
    , _env_repo_var   = repo_var
    , _env_repo_saver = repo_saver
    , _env_manager    = manager
    , _env_scrapers   = scrapers_env
    , _env_self_url   = self_url
    }

data DevEnv = DevEnv
  { _dev_env_conn       :: !Connection
  , _dev_env_repo_var   :: !(MVar NgramsRepo)
  , _dev_env_repo_saver :: !(IO ())
  }

makeLenses ''DevEnv

instance HasConnection DevEnv where
  connection = dev_env_conn

instance HasRepoVar DevEnv where
  repoVar = dev_env_repo_var

instance HasRepoSaver DevEnv where
  repoSaver = dev_env_repo_saver

newDevEnvWith :: FilePath -> IO DevEnv
newDevEnvWith file = do
  param      <- databaseParameters file
  conn       <- connect param
  repo_var   <- readRepo
  repo_saver <- mkRepoSaver repo_var
  pure $ DevEnv
    { _dev_env_conn       = conn
    , _dev_env_repo_var   = repo_var
    , _dev_env_repo_saver = repo_saver
    }

-- | Run Cmd Sugar for the Repl (GHCI)
runCmdRepl :: Show err => Cmd' DevEnv err a -> IO a
runCmdRepl f = newDevEnv >>= \env -> runCmdDev env f

newDevEnv :: IO DevEnv
newDevEnv = newDevEnvWith "gargantext.ini"

-- Use only for dev
-- In particular this writes the repo file after running
-- the command.
-- This function is constrained to the DevEnv rather than
-- using HasConnection and HasRepoVar.
runCmdDev :: Show err => DevEnv -> Cmd' DevEnv err a -> IO a
runCmdDev env f =
  (either (fail . show) pure =<< runCmd env f)
    `finally`
  runReaderT saveRepo env

-- Use only for dev
runCmdDevNoErr :: DevEnv -> Cmd' DevEnv () a -> IO a
runCmdDevNoErr = runCmdDev

-- Use only for dev
runCmdDevServantErr :: DevEnv -> Cmd' DevEnv ServantErr a -> IO a
runCmdDevServantErr = runCmdDev
