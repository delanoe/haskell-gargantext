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

import Data.Either
import Prelude (read)
import Control.Exception (finally)
import Gargantext.Prelude
import Gargantext.Database.Flow (FlowCmdM, flowCorpusFile, flowAnnuaire)
import Gargantext.Text.Corpus.Parsers (FileFormat(..))
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId, toHyperdataDocument)
import Gargantext.Database.Schema.User (insertUsersDemo)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Core (Lang(..))
import Gargantext.API.Types (GargError)
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (withDevEnv, runCmdDev, DevEnv)
import System.Environment (getArgs)
--import Gargantext.Text.Corpus.Parsers.GrandDebat (readFile, GrandDebatReference(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  [fun, user, name, iniPath, limit, corpusPath] <- getArgs

  --{-

  let createUsers :: Cmd GargError Int64
      createUsers = insertUsersDemo
  
  let
    --tt = (Unsupervised EN 6 0 Nothing)
    tt = (Multi EN)
    format = CsvGargV3 -- CsvHalFormat --WOS
    corpus :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
    corpus = flowCorpusFile (cs user) (Left (cs name :: Text)) (read limit :: Int) tt  format corpusPath

    annuaire :: forall m. FlowCmdM DevEnv GargError m => m CorpusId
    annuaire = flowAnnuaire (cs user) (Left "Annuaire") (Multi EN) corpusPath


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
