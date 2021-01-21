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
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Query.Table.Node.UpdateOpaleye
import Gargantext.Database.Prelude (Cmd'', )
import Gargantext.Prelude
import System.Environment (getArgs)

-- | PosTag
import Gargantext.Database.Action.Flow (indexAllDocumentsWithPosTag)
import Gargantext.Database.Query.Table.NgramsPostag (createTable_NgramsPostag)

main :: IO ()
main = do
  [iniPath] <- getArgs

  let
    upgrade :: Cmd'' DevEnv GargError ()
    upgrade = do
      _ <- createTable_NgramsPostag
      _ <- indexAllDocumentsWithPosTag
      pure ()

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env upgrade
    putStrLn "Uprade"
  pure ()
