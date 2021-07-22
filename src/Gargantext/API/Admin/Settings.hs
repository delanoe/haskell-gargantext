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



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Settings
    where

import Codec.Serialise (Serialise(), serialise, deserialise)
import Control.Concurrent
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceFreq, debounceAction)
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, connect, close, ConnectInfo)
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Admin.Types
import Gargantext.API.Ngrams.Types (NgramsRepo, HasRepo(..), RepoEnv(..), r_version, initRepo, renv_var, renv_lock)
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude (databaseParameters, HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), gc_repofilepath, readConfig)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Auth.Server (defaultJWTSettings, CookieSettings(..), XsrfCookieSettings(..), defaultCookieSettings, defaultXsrfCookieSettings, readKey, writeKey)
import Servant.Client (parseBaseUrl)
import Servant.Job.Async (newJobEnv, defaultSettings)
import System.Directory
import System.FileLock (tryLockFile, unlockFile, SharedExclusive(Exclusive))
import System.IO (FilePath, hClose)
import System.IO.Temp (withTempFile)
import System.Log.FastLogger
import qualified Data.ByteString.Lazy as L

devSettings :: FilePath -> IO Settings
devSettings jwkFile = do
  jwkExists <- doesFileExist jwkFile
  when (not jwkExists) $ writeKey jwkFile
  jwk       <- readKey jwkFile
  pure $ Settings
    { _allowedOrigin = "http://localhost:8008"
    , _allowedHost = "localhost:3000"
    , _appPort = 3000
    , _logLevelLimit = LevelDebug
--    , _dbServer = "localhost"
    , _sendLoginEmails = LogEmailToConsole
    , _scrapydUrl = fromMaybe (panic "Invalid scrapy URL") $ parseBaseUrl "http://localhost:6800"
    , _cookieSettings = defaultCookieSettings { cookieXsrfSetting = Just xsrfCookieSetting } -- TODO-SECURITY tune
    , _jwtSettings = defaultJWTSettings jwk -- TODO-SECURITY tune
    }
  where
    xsrfCookieSetting = defaultXsrfCookieSettings { xsrfExcludeGet = True }

{- NOT USED YET
import System.Environment (lookupEnv)

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

settingsFromEnvironment :: IO Settings
settingsFromEnvironment =
    Settings <$> (encodeUtf8 <$> reqSetting "ALLOWED_ORIGIN")
             <*> (encodeUtf8 <$> reqSetting "ALLOWED_HOST")
             <*> optSetting "PORT" 3000
             <*> (parseLogLevel <$> optSetting "LOG_LEVEL" "warn")
             <*> reqSetting "DB_SERVER"
             <*> (parseJwk <$> reqSetting "JWT_SECRET")
             <*> optSetting "SEND_EMAIL" SendEmailViaAws
-}

-----------------------------------------------------------------------
-- | RepoDir FilePath configuration
type RepoDirFilePath = FilePath

repoSnapshot :: RepoDirFilePath -> FilePath
repoSnapshot repoDir = repoDir <> "/repo.cbor"



-- This assumes we own the lock on repoSnapshot.
repoSaverAction :: RepoDirFilePath -> Serialise a => a -> IO ()
repoSaverAction repoDir a = do
  withTempFile repoDir "tmp-repo.cbor" $ \fp h -> do
    printDebug "repoSaverAction" fp
    L.hPut h $ serialise a
    hClose h
    renameFile fp (repoSnapshot repoDir)



-- The use of mkDebounce makes sure that repoSaverAction is not called too often.
-- If repoSaverAction start taking more time than the debounceFreq then it should
-- be increased.
mkRepoSaver :: RepoDirFilePath -> MVar NgramsRepo -> IO (IO ())
mkRepoSaver repoDir repo_var = mkDebounce settings'
  where
    settings' = defaultDebounceSettings
                 { debounceFreq   = let n = 6 :: Int in 10^n  -- 1 second
                 , debounceAction = withMVar repo_var (repoSaverAction repoDir)
                   -- Here this not only `readMVar` but `takeMVar`.
                   -- Namely while repoSaverAction is saving no other change
                   -- can be made to the MVar.
                   -- This might be not efficent and thus reconsidered later.
                   -- However this enables to safely perform a *final* save.
                   -- See `cleanEnv`.
                   -- Future work:
                   -- Add a new MVar just for saving.
                 }

readRepoEnv :: FilePath -> IO RepoEnv
readRepoEnv repoDir = do
  -- Does file exist ? :: Bool
  _repoDir <- createDirectoryIfMissing True repoDir

  repoFile <- doesFileExist (repoSnapshot repoDir)

  -- Is file not empty ? :: Bool
  repoExists <- if repoFile
             then (>0) <$> getFileSize (repoSnapshot repoDir)
             else pure False

  mlock <- tryLockFile (repoSnapshot repoDir) Exclusive
  lock <- maybe (panic "Repo file already locked") pure mlock

  mvar <- newMVar =<<
    if repoExists
      then do
        -- e_repo <- eitherDecodeStrict <$> deserialise <$> L.readFile repoSnapshot
        repo <- deserialise <$> L.readFile (repoSnapshot repoDir)
        -- repo   <- either fail pure e_repo
        let archive = (repoSnapshot repoDir) <> ".v" <> show (repo ^. r_version)
        copyFile (repoSnapshot repoDir) archive
        pure repo
      else
        pure initRepo
  -- TODO save in DB here
  saver <- mkRepoSaver repoDir mvar
  pure $ RepoEnv { _renv_var = mvar, _renv_saver = saver, _renv_lock = lock }

devJwkFile :: FilePath
devJwkFile = "dev.jwk"

newEnv :: PortNumber -> FilePath -> IO Env
newEnv port file = do
  manager_env  <- newTlsManager
  settings'    <- devSettings devJwkFile <&> appPort .~ port -- TODO read from 'file'
  when (port /= settings' ^. appPort) $
    panic "TODO: conflicting settings of port"

  config_env    <- readConfig file
  self_url_env  <- parseBaseUrl $ "http://0.0.0.0:" <> show port
  dbParam       <- databaseParameters file
  pool          <- newPool dbParam
  repo          <- readRepoEnv (_gc_repofilepath config_env)
  nodeStory_env <- readNodeStoryEnv (_gc_repofilepath config_env)
  scrapers_env  <- newJobEnv defaultSettings manager_env
  logger        <- newStderrLoggerSet defaultBufSize

  pure $ Env
    { _env_settings  = settings'
    , _env_logger    = logger
    , _env_pool      = pool
    , _env_repo      = repo
    , _env_nodeStory = nodeStory_env
    , _env_manager   = manager_env
    , _env_scrapers  = scrapers_env
    , _env_self_url  = self_url_env
    , _env_config    = config_env
    }

newPool :: ConnectInfo -> IO (Pool Connection)
newPool param = createPool (connect param) close 1 (60*60) 8

cleanEnv :: (HasConfig env, HasRepo env) => env -> IO ()
cleanEnv env = do
  r <- takeMVar (env ^. repoEnv . renv_var)
  repoSaverAction (env ^. hasConfig . gc_repofilepath) r
  unlockFile (env ^. repoEnv . renv_lock)

