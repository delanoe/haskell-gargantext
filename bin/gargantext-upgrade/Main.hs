{-|
Module      : Main.hs
Description : Gargantext Import Corpus
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Import a corpus binary.

 -}

{-# LANGUAGE Strict            #-}

module Main where

import Gargantext.API.Admin.EnvTypes (DevEnv)
import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Prelude (GargError)
import Gargantext.API.Node () -- instances only
import Gargantext.API.Ngrams.Tools (getRepo)
import Gargantext.Database.Prelude (Cmd'', )
import Gargantext.Core.NodeStory
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import System.Environment (getArgs)
import Prelude (getLine)
import GHC.IO.Exception (IOException)

main :: IO ()
main = do

  putStrLn "Manual method:"
  putStrLn "Upgrade your GarganText instance with the script:"
  putStrLn "Then press enter key to launch upgrade."
  _ok  <- getLine

  [iniPath] <- getArgs
  cfg       <- readConfig         iniPath

  let
    -- upgrade :: Cmd'' DevEnv GargError ()
    upgrade :: Cmd'' DevEnv IOException ()
    upgrade = do
      let repo_filepath = _gc_repofilepath cfg
      repo <- getRepo
      _ <- liftBase $ repoMigration repo_filepath repo
      pure ()
      

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env upgrade
    putStrLn "Uprade done with success"
  pure ()
