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

import System.Environment (getArgs)
import Gargantext.Prelude
import Gargantext.Database.Flow (FlowCmdM, flowCorpusFile, getOrMkRoot)
import Gargantext.Text.Corpus.Parsers (FileFormat(..))
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId, toHyperdataDocument, RootId)
import Gargantext.Database.Schema.User (insertUsersDemo, UserId)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Core (Lang(..))
import Gargantext.API.Types (GargError)
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (withDevEnv, runCmdDev, DevEnv)
--import Gargantext.Text.Corpus.Parsers.GrandDebat (readFile, GrandDebatReference(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  [iniPath] <- getArgs

  let createUsers :: Cmd GargError Int64
      createUsers = insertUsersDemo
  
  let
    mkRoots :: Cmd GargError (UserId, RootId)
    mkRoots = getOrMkRoot "user1"


  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env createUsers
    _ <- runCmdDev env mkRoots
    pure ()
