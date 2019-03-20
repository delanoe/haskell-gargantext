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
import Gargantext.Database.Flow (FlowCmdM, flowCorpus'')
import Gargantext.Text.Parsers (FileFormat(CsvHalFormat))
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId)
import Gargantext.Database.Schema.User (insertUsersDemo)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Core (Lang(..))
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (newDevEnvWith, runCmdDev, DevEnv)
import System.Environment (getArgs)
import Gargantext.Text.Parsers.GrandDebat (readFile, GrandDebatReference(..))
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  [user, iniPath, name, corpusPath, users] <- getArgs

  --{-
  let createUsers :: Cmd ServantErr Int64
      createUsers = insertUsersDemo
  {-
  let cmdCorpus :: forall m. FlowCmdM DevEnv ServantErr m => m CorpusId
      cmdCorpus = flowCorpus (cs user) (cs name) (Mono EN) CsvHalFormat corpusPath
  --}
  let cmdCorpus :: forall m. FlowCmdM DevEnv ServantErr m => m [CorpusId]
      cmdCorpus = do
        docs <- liftIO (splitEvery 500 <$> readFile corpusPath :: IO [[GrandDebatReference ]])
        ids <- flowCorpus'' (Text.pack user) (Text.pack name) (Mono FR) docs
        pure ids

     -- cmd = {-createUsers >>-} cmdCorpus

  env <- newDevEnvWith iniPath
  -- Better if we keep only one call to runCmdDev.
  _ <- if users == "0"
        then runCmdDev env createUsers
        else pure 1
  _ <- runCmdDev env cmdCorpus
  pure ()


