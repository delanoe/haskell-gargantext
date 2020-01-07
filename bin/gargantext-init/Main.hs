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

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import System.Environment (getArgs)
import Gargantext.Prelude
import Gargantext.Database.Flow (getOrMkRoot, getOrMkRootWithCorpus)
import Gargantext.Database.Schema.Node (getOrMkList)
import Gargantext.Database.Utils (Cmd, )
import Gargantext.Database.Types.Node (CorpusId, RootId, HyperdataCorpus, ListId)
import Gargantext.Database.Schema.User (insertUsersDemo, UserId)
import Gargantext.API.Types (GargError)
import Gargantext.API.Node () -- instances
import Gargantext.API.Settings (withDevEnv, runCmdDev)
import Gargantext.Database.Config (userMaster, corpusMasterName)
import Gargantext.Database.Init (initTriggers)
main :: IO ()
main = do
  [iniPath] <- getArgs

  let createUsers :: Cmd GargError Int64
      createUsers = insertUsersDemo

  let
    mkRoots :: Cmd GargError [(UserId, RootId)]
    mkRoots = mapM getOrMkRoot ["gargantua", "user1", "user2"]
    -- TODO create all users roots

  let
    initMaster :: Cmd GargError (UserId, RootId, CorpusId, ListId)
    initMaster = do
      (masterUserId, masterRootId, masterCorpusId) <- getOrMkRootWithCorpus userMaster (Left corpusMasterName) (Nothing :: Maybe HyperdataCorpus)
      masterListId <- getOrMkList masterCorpusId masterUserId
      _ <- initTriggers masterListId
      pure (masterUserId, masterRootId, masterCorpusId, masterListId)

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env createUsers
    _ <- runCmdDev env mkRoots
    x <- runCmdDev env initMaster
    putStrLn $ show x
    pure ()
