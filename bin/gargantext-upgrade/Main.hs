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

import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Prelude (GargError)
import Gargantext.API.Node () -- instances only
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Query.Table.Node.UpdateOpaleye
import Gargantext.Database.Prelude (Cmd, )
import Gargantext.Prelude
import System.Environment (getArgs)





main :: IO ()
main = do
  [iniPath] <- getArgs

  let
    updateNodes :: Cmd GargError [Int64]
    updateNodes = updateNodesWithType_
                    NodeList
                    defaultHyperdataList

  withDevEnv iniPath $ \env -> do
    x <- runCmdDev env updateNodes
    putStrLn $ show x
  pure ()
