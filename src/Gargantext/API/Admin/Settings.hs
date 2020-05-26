{-| 
Module      : Gargantext.API.Admin.Settings
Description : Settings of the API (Server and Client)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO-SECURITY: Critical
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Settings
    where

import Codec.Serialise (Serialise(), serialise, deserialise)
import Control.Concurrent
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceFreq, debounceAction)
import Control.Exception (finally)
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool)
import Data.Text
import Database.PostgreSQL.Simple (Connection, connect, close, ConnectInfo)
import GHC.Enum
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (NgramsRepo, HasRepoVar(..), HasRepoSaver(..), HasRepo(..), RepoEnv(..), r_version, saveRepo, initRepo, renv_var, renv_lock)
import Gargantext.Database.Prelude (databaseParameters, HasConnectionPool(..), Cmd', runCmd)
import Gargantext.Prelude
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Prelude (Bounded(), fail)
import Servant
import Servant.Auth.Server (defaultJWTSettings, JWTSettings, CookieSettings(..), XsrfCookieSettings(..), defaultCookieSettings, defaultXsrfCookieSettings, readKey, writeKey)
import Servant.Client (BaseUrl, parseBaseUrl)
import Servant.Job.Async (newJobEnv, defaultSettings, HasJobEnv(..), Job)
import System.Directory
import System.Environment (lookupEnv)
import System.FileLock (tryLockFile, unlockFile, SharedExclusive(Exclusive))
import System.IO (FilePath, hClose)
import System.IO.Temp (withTempFile)
import System.Log.FastLogger
import Web.HttpApiData (parseUrlPiece)
import qualified Data.ByteString.Lazy as L
import qualified Servant.Job.Core


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
    , _fileFolder      :: FilePath
    }

makeLenses ''Settings

class HasSettings env where
  settings :: Getter env Settings

devSettings :: FilePath -> IO Settings
devSettings jwkFile = do
  jwkExists <- doesFileExist jwkFile
  when (not jwkExists) $ writeKey jwkFile
  jwk <- readKey jwkFile
  pure $ Settings
    { _allowedOrigin = "http://localhost:8008"
    , _allowedHost = "localhost:3000"
    , _appPort = 3000
    , _logLevelLimit = LevelDebug
--    , _dbServer = "localhost"
    , _sendLoginEmails = LogEmailToConsole
    , _scrapydUrl = fromMaybe (panic "Invalid scrapy URL") $ parseBaseUrl "http://localhost:6800"
    , _fileFolder = "data"
    , _cookieSettings = defaultCookieSettings { cookieXsrfSetting = Just xsrfCookieSetting } -- TODO-SECURITY tune
    , _jwtSettings = defaultJWTSettings jwk -- TODO-SECURITY tune
    }
  where
    xsrfCookieSetting = defaultXsrfCookieSettings { xsrfExcludeGet = True }



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
  { _env_settings :: !Settings
  , _env_logger   :: !LoggerSet
  , _env_pool     :: !(Pool Connection)
  , _env_repo     :: !RepoEnv
  , _env_manager  :: !Manager
  , _env_self_url :: !BaseUrl
  , _env_scrapers :: !ScrapersEnv
  }
  deriving (Generic)

makeLenses ''Env

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

instance Servant.Job.Core.HasEnv Env (Job ScraperStatus ScraperStatus) where
  _env = env_scrapers . Servant.Job.Core._env

instance HasJobEnv Env ScraperStatus ScraperStatus where
  job_env = env_scrapers

data MockEnv = MockEnv
  { _menv_firewall :: !FireWall
  }
  deriving (Generic)

makeLenses ''MockEnv

-- | TODO add this path in Settings

repoDir :: FilePath
repoDir = "repos"

repoSnapshot :: FilePath
repoSnapshot = repoDir <> "/repo.json"

-- | TODO add hard coded file in Settings
-- This assumes we own the lock on repoSnapshot.
repoSaverAction :: Serialise a => a -> IO ()
repoSaverAction a = do
  withTempFile "repos" "tmp-repo.json" $ \fp h -> do
    printDebug "repoSaverAction" fp
    L.hPut h $ serialise a
    hClose h
    renameFile fp repoSnapshot

mkRepoSaver :: MVar NgramsRepo -> IO (IO ())
mkRepoSaver repo_var = mkDebounce settings
  where
    settings = defaultDebounceSettings
                 { debounceFreq   = 1000000 -- 1 second
                 , debounceAction = withMVar repo_var repoSaverAction
                   -- Here this not only `readMVar` but `takeMVar`.
                   -- Namely while repoSaverAction is saving no other change
                   -- can be made to the MVar.
                   -- This might be not efficent and thus reconsidered later.
                   -- However this enables to safely perform a *final* save.
                   -- See `cleanEnv`.
                   -- Future work:
                   -- Add a new MVar just for saving.
                 }

readRepoEnv :: IO RepoEnv
readRepoEnv = do
  -- Does file exist ? :: Bool
  _repoDir <- createDirectoryIfMissing True repoDir

  repoFile <- doesFileExist repoSnapshot

  -- Is file not empty ? :: Bool
  repoExists <- if repoFile
             then (>0) <$> getFileSize repoSnapshot
             else pure False

  mlock <- tryLockFile repoSnapshot Exclusive
  lock <- maybe (panic "Repo file already locked") pure mlock

  mvar <- newMVar =<<
    if repoExists
      then do
        -- e_repo <- eitherDecodeStrict <$> deserialise <$> L.readFile repoSnapshot
        repo <- deserialise <$> L.readFile repoSnapshot
        -- repo   <- either fail pure e_repo
        let archive = repoSnapshot <> ".v" <> show (repo ^. r_version)
        copyFile repoSnapshot archive
        pure repo
      else
        pure initRepo
  -- TODO save in DB here
  saver <- mkRepoSaver mvar
  pure $ RepoEnv { _renv_var = mvar, _renv_saver = saver, _renv_lock = lock }

devJwkFile :: FilePath
devJwkFile = "dev.jwk"

newEnv :: PortNumber -> FilePath -> IO Env
newEnv port file = do
  manager <- newTlsManager
  settings <- devSettings devJwkFile <&> appPort .~ port -- TODO read from 'file'
  when (port /= settings ^. appPort) $
    panic "TODO: conflicting settings of port"

  self_url <- parseBaseUrl $ "http://0.0.0.0:" <> show port
  param    <- databaseParameters file
  pool     <- newPool param
  repo     <- readRepoEnv
  scrapers_env <- newJobEnv defaultSettings manager
  logger <- newStderrLoggerSet defaultBufSize

  pure $ Env
    { _env_settings   = settings
    , _env_logger     = logger
    , _env_pool       = pool
    , _env_repo       = repo
    , _env_manager    = manager
    , _env_scrapers   = scrapers_env
    , _env_self_url   = self_url
    }

newPool :: ConnectInfo -> IO (Pool Connection)
newPool param = createPool (connect param) close 1 (60*60) 8

data DevEnv = DevEnv
  { _dev_env_pool :: !(Pool Connection)
  , _dev_env_repo :: !RepoEnv
  , _dev_env_settings :: !Settings
  }

makeLenses ''DevEnv

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

cleanEnv :: HasRepo env => env -> IO ()
cleanEnv env = do
  r <- takeMVar (env ^. repoEnv . renv_var)
  repoSaverAction r
  unlockFile (env ^. repoEnv . renv_lock)

withDevEnv :: FilePath -> (DevEnv -> IO a) -> IO a
withDevEnv iniPath k = do
  env <- newDevEnv
  k env `finally` cleanEnv env

  where
    newDevEnv = do
      param <- databaseParameters iniPath
      pool  <- newPool param
      repo  <- readRepoEnv
      setts <- devSettings devJwkFile
      pure $ DevEnv
        { _dev_env_pool = pool
        , _dev_env_repo = repo
        , _dev_env_settings = setts
        }

-- | Run Cmd Sugar for the Repl (GHCI)
runCmdRepl :: Show err => Cmd' DevEnv err a -> IO a
runCmdRepl f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

runCmdReplServantErr :: Cmd' DevEnv ServerError a -> IO a
runCmdReplServantErr = runCmdRepl

-- Use only for dev
-- In particular this writes the repo file after running
-- the command.
-- This function is constrained to the DevEnv rather than
-- using HasConnectionPool and HasRepoVar.
runCmdDev :: Show err => DevEnv -> Cmd' DevEnv err a -> IO a
runCmdDev env f =
  (either (fail . show) pure =<< runCmd env f)
    `finally`
  runReaderT saveRepo env

-- Use only for dev
runCmdDevNoErr :: DevEnv -> Cmd' DevEnv () a -> IO a
runCmdDevNoErr = runCmdDev

-- Use only for dev
runCmdDevServantErr :: DevEnv -> Cmd' DevEnv ServerError a -> IO a
runCmdDevServantErr = runCmdDev
