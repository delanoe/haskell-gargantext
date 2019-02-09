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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Strict            #-}

module Main where

import Control.Exception (finally)
import Servant (ServantErr)
import Gargantext.Prelude
import Gargantext.Database.Flow (FlowCmdM, flowCorpus)
import Gargantext.Text.Parsers (FileFormat(CsvHalFormat))
import Gargantext.Database.Utils (Cmd, connectGargandb, runCmdDev)
import Gargantext.Database.Types.Node (CorpusId)
--import Gargantext.Database.Schema.User (insertUsers, gargantuaUser, simpleUser)
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (newDevEnvWith, cleanEnv, DevEnv)
import System.Environment (getArgs)

main :: IO ()
main = do
  [iniPath, name, corpusPath] <- getArgs

  env <- newDevEnvWith iniPath

  (do
    {-let createUsers :: Cmd ServantErr Int64
        createUsers = insertUsers [gargantuaUser,simpleUser]
    _ <- runCmdDev env createUsers
    -}

    let cmd :: FlowCmdM DevEnv ServantErr m => m CorpusId
        cmd = flowCorpus CsvHalFormat corpusPath (cs name)
    _ <- runCmdDev env cmd
    pure ()
    ) `finally` cleanEnv env


