{-|
Module      : Gargantext.API.Dev
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

-- Use only for dev/repl
module Gargantext.API.Dev where

import Control.Exception (finally)
import Control.Monad (fail)
import Control.Monad.Reader (runReaderT)
import Servant

import Gargantext.API.Prelude
import Gargantext.API.Admin.Settings
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Ngrams (saveRepo)
import Gargantext.Database.Prelude
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import System.IO (FilePath)

type IniPath  = FilePath
-------------------------------------------------------------------
withDevEnv :: IniPath -> (DevEnv -> IO a) -> IO a
withDevEnv iniPath k = do
  env <- newDevEnv
  k env `finally` cleanEnv env

  where
    newDevEnv = do
      cfg     <- readConfig         iniPath
      dbParam <- databaseParameters iniPath
      pool    <- newPool            dbParam
      repo    <- readRepoEnv        (_gc_repofilepath cfg)
      setts   <- devSettings devJwkFile
      pure $ DevEnv
        { _dev_env_pool     = pool
        , _dev_env_repo     = repo
        , _dev_env_settings = setts
        , _dev_env_config   = cfg
        }

-- | Run Cmd Sugar for the Repl (GHCI)
runCmdRepl :: Show err => Cmd'' DevEnv err a -> IO a
runCmdRepl f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

runCmdReplServantErr :: Cmd'' DevEnv ServerError a -> IO a
runCmdReplServantErr = runCmdRepl

-- In particular this writes the repo file after running
-- the command.
-- This function is constrained to the DevEnv rather than
-- using HasConnectionPool and HasRepoVar.
runCmdDev :: Show err => DevEnv -> Cmd'' DevEnv err a -> IO a
runCmdDev env f =
  (either (fail . show) pure =<< runCmd env f)
    `finally`
  runReaderT saveRepo env

runCmdDevNoErr :: DevEnv -> Cmd' DevEnv () a -> IO a
runCmdDevNoErr = runCmdDev

runCmdDevServantErr :: DevEnv -> Cmd' DevEnv ServerError a -> IO a
runCmdDevServantErr = runCmdDev

runCmdReplEasy :: Cmd'' DevEnv GargError a -> IO a
runCmdReplEasy f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f
