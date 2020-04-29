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
import Data.Either
import Data.Text (Text)
import Gargantext.API.Node () -- instances
import Gargantext.API.Admin.Settings (withDevEnv, runCmdDev, DevEnv)
import Gargantext.API.Admin.Types (GargError)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (FlowCmdM, flowCorpusFile, flowAnnuaire, TermType(..))
import Gargantext.Database.Query.Table.User (insertUsersDemo)
import Gargantext.Database.Admin.Types.Node (CorpusId, toHyperdataDocument)
import Gargantext.Database.Admin.Utils (Cmd, )
import Gargantext.Prelude
import Gargantext.Text.Corpus.Parsers (FileFormat(..))
import Prelude (read)
import System.Environment (getArgs)
import qualified Data.Text as Text

main :: IO ()
main = do
  [fun, user, name, iniPath, limit, corpusPath] <- getArgs

  --{-

  let createUsers :: Cmd GargError Int64
      createUsers = insertUsersDemo
  
  let
    --tt = (Unsupervised EN 6 0 Nothing)
    tt = (Multi EN)
    format = CsvGargV3 -- CsvHal --WOS
    corpus :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
    corpus = flowCorpusFile (UserName $ cs user) (Left (cs name :: Text)) (read limit :: Int) tt  format corpusPath

    corpusCsvHal :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
    corpusCsvHal = flowCorpusFile (UserName $ cs user) (Left (cs name :: Text)) (read limit :: Int) tt CsvHal corpusPath

    annuaire :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
    annuaire = flowAnnuaire (UserName $ cs user) (Left "Annuaire") (Multi EN) corpusPath


  {-
  let debatCorpus :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
      debatCorpus = do
        docs <- liftIO ( splitEvery 500
                       <$> take (read limit :: Int)
                       <$> readFile corpusPath
                       :: IO [[GrandDebatReference ]]
                       )
        flowCorpus (Text.pack user) (Text.pack name) (Multi FR) (map (map toHyperdataDocument) docs)
  --}

  withDevEnv iniPath $ \env -> do
    _ <- if fun == "users"
          then runCmdDev env createUsers
          else pure 0 --(cs "false")

    _ <- if fun == "corpus"
          then runCmdDev env corpus
          else pure 0 --(cs "false")

    _ <- if fun == "corpusCsvHal"
          then runCmdDev env corpusCsvHal
          else pure 0 --(cs "false")
 

    _ <- if fun == "annuaire"
            then runCmdDev env annuaire
            else pure 0
    {-
    _ <- if corpusType == "csv"
            then runCmdDev env csvCorpus
            else if corpusType == "debat"
              then runCmdDev env debatCorpus
              else panic "corpusType unknown: try \"csv\" or \"debat\""
    -}
    pure ()
