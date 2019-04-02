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

import Prelude (read)
import Control.Exception (finally)
import Servant (ServantErr)
import Gargantext.Prelude
import Gargantext.Database.Flow (FlowCmdM, flowCorpus)
import Gargantext.Text.Parsers (FileFormat(CsvHalFormat))
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId, toHyperdataDocument)
import Gargantext.Database.Schema.User (insertUsersDemo)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Core (Lang(..))
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (withDevEnv, runCmdDev, DevEnv)
import System.Environment (getArgs)
import Gargantext.Text.Parsers.GrandDebat (readFile, GrandDebatReference(..))
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  [userCreate, user, name, iniPath, limit, corpusPath] <- getArgs

  --{-
  let createUsers :: Cmd ServantErr Int64
      createUsers = insertUsersDemo
  {-
  let csvCorpus :: forall m. FlowCmdM DevEnv ServantErr m => m CorpusId
      csvCorpus = flowCorpus (cs user) (cs name) (Multi EN) CsvHalFormat corpusPath
  --}
  let debatCorpus :: forall m. FlowCmdM DevEnv ServantErr m => m CorpusId
      debatCorpus = do
        docs <- liftIO ( splitEvery 500
                       <$> take (read limit :: Int)
                       <$> readFile corpusPath
                       :: IO [[GrandDebatReference ]]
                       )
        flowCorpus (Text.pack user) (Text.pack name) (Multi FR) (map (map toHyperdataDocument) docs)


  withDevEnv iniPath $ \env -> do
    _ <- if userCreate == "true"
          then runCmdDev env createUsers
          else pure 0 --(cs "false")

    _ <- runCmdDev env debatCorpus
    {-
    _ <- if corpusType == "csv"
            then runCmdDev env csvCorpus
            else if corpusType == "debat"
              then runCmdDev env debatCorpus
              else panic "corpusType unknown: try \"csv\" or \"debat\""
    -}
    pure ()
