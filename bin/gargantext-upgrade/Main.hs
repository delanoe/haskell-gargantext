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
import Gargantext.Database.Prelude (Cmd'', )
import Gargantext.Prelude
import System.Environment (getArgs)
import Prelude (getLine)

-- PosTag
import Gargantext.Database.Action.Flow (indexAllDocumentsWithPosTag)

main :: IO ()
main = do
  [iniPath] <- getArgs

  putStrLn "Manual method (for now):"
  putStrLn "Upgrade your schema database with the script:"
  putStrLn "psql gargandbV5 < ./devops/postgres/upgrade/0.0.2.6.sql"
  putStrLn "Then press enter key when you are done"
  _ok  <- getLine

  let
    upgrade :: Cmd'' DevEnv GargError ()
    upgrade = do
      -- This method does not work for now
      -- _ <- createTable_NgramsPostag
      _ <- indexAllDocumentsWithPosTag
      pure ()

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env upgrade
    putStrLn "Uprade"
  pure ()
