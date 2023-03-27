{-|
Module      : Gargantext.API.Dev
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}

-- Use only for dev/repl
module Gargantext.API.Dev where

import Control.Exception (finally)
import Control.Monad (fail)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Admin.Settings
import Gargantext.API.Ngrams (saveNodeStoryImmediate)
import Gargantext.API.Prelude
import Gargantext.Core.NLP (nlpServerMap)
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude
import Gargantext.Prelude
import Gargantext.Prelude.Config (readConfig)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Gargantext.Prelude.Mail as Mail
import qualified Gargantext.Prelude.NLP as NLP
import qualified Gargantext.Utils.Jobs.Monad as Jobs
import qualified Gargantext.Utils.Jobs.Queue as Jobs
import Servant
import System.IO (FilePath)

type IniPath  = FilePath
-------------------------------------------------------------------
withDevEnv :: IniPath -> (DevEnv -> IO a) -> IO a
withDevEnv iniPath k = do
  env <- newDevEnv
  k env -- `finally` cleanEnv env

  where
    newDevEnv = do
      cfg     <- readConfig         iniPath
      dbParam <- databaseParameters iniPath
      --nodeStory_env <- readNodeStoryEnv (_gc_repofilepath cfg)
      pool    <- newPool            dbParam
      nodeStory_env <- readNodeStoryEnv pool
      setts   <- devSettings devJwkFile
      mail    <- Mail.readConfig iniPath
      nlp_config <- NLP.readConfig iniPath
      secret        <- Jobs.genSecret
      let jobs_settings = Jobs.defaultJobSettings 1 secret
      manager_env   <- newTlsManager
      jobs_env      <- Jobs.newJobEnv jobs_settings Jobs.defaultPrios manager_env
      pure $ DevEnv
        { _dev_env_pool     = pool
        , _dev_env_nodeStory  = nodeStory_env
        , _dev_env_settings = setts
        , _dev_env_config   = cfg
        , _dev_env_mail     = mail
        , _dev_env_nlp      = nlpServerMap nlp_config
        , _dev_env_jobs     = jobs_env
        }

type DevCmd env err a = forall m. (
    CmdM'' env err m
  , Jobs.MonadJobStatus m
  ) => m a

-- | Run Cmd Sugar for the Repl (GHCI)
runCmdRepl :: Show err => Cmd'' DevEnv err a -> IO a
runCmdRepl f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

runCmdReplServantErr :: Cmd'' DevEnv ServerError a -> IO a
runCmdReplServantErr = runCmdRepl

-- In particular this writes the repo file after running
-- the command.
-- This function is constrained to the DevEnv rather than
-- using HasConnectionPool and HasRepoVar.
runCmdDev :: Show err => DevEnv -> DevCmd DevEnv err a -> IO a
runCmdDev env cmd =
  (either (fail . show) pure =<< runExceptT (runReaderT cmd env))
    `finally`
  runReaderT saveNodeStoryImmediate env

runCmdGargDev :: DevEnv -> GargM DevEnv GargError a -> IO a
runCmdGargDev env cmd =
  (either (fail . show) pure =<< runExceptT (runReaderT cmd env))
    `finally`
  runReaderT saveNodeStoryImmediate env

runCmdDevNoErr :: DevEnv -> Cmd' DevEnv () a -> IO a
runCmdDevNoErr = runCmdDev

runCmdDevServantErr :: DevEnv -> Cmd' DevEnv ServerError a -> IO a
runCmdDevServantErr = runCmdDev

runCmdReplEasy :: Cmd'' DevEnv GargError a -> IO a
runCmdReplEasy f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f
