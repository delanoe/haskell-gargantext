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
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId)
--import Gargantext.Database.Schema.User (insertUsers, gargantuaUser, simpleUser)
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (newDevEnvWith, runCmdDev, DevEnv)
import System.Environment (getArgs)

main :: IO ()
main = do
  [user, iniPath, name, corpusPath] <- getArgs

  {-let createUsers :: Cmd ServantErr Int64
      createUsers = insertUsers [gargantuaUser,simpleUser]
  -}

  let cmdCorpus :: forall m. FlowCmdM DevEnv ServantErr m => m CorpusId
      cmdCorpus = flowCorpus (cs user) (cs name) CsvHalFormat corpusPath

     -- cmd = {-createUsers >>-} cmdCorpus

  env <- newDevEnvWith iniPath
  -- Better if we keep only one call to runCmdDev.
  _ <- runCmdDev env cmdCorpus
  pure ()


