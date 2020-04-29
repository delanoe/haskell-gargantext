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
import Gargantext.API.Admin.Settings (withDevEnv, runCmdDev)
import Gargantext.API.Admin.Types (GargError)
import Gargantext.API.Node () -- instances
import Gargantext.Core.Types.Individu (UserId, User(..))
import Gargantext.Database.Action.Flow (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Query.Table.Node (getOrMkList)
import Gargantext.Database.Query.Table.User (insertUsersDemo)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Trigger.Init (initTriggers)
import Gargantext.Database.Admin.Types.Node (CorpusId, RootId, HyperdataCorpus, ListId)
import Gargantext.Database.Prelude (Cmd, )
import Gargantext.Prelude
import System.Environment (getArgs)

main :: IO ()
main = do
  [iniPath] <- getArgs

  let createUsers :: Cmd GargError Int64
      createUsers = insertUsersDemo

  let
    mkRoots :: Cmd GargError [(UserId, RootId)]
    mkRoots = mapM getOrMkRoot $ map UserName ["gargantua", "user1", "user2"]
    -- TODO create all users roots

  let
    initMaster :: Cmd GargError (UserId, RootId, CorpusId, ListId)
    initMaster = do
      (masterUserId, masterRootId, masterCorpusId) <- getOrMk_RootWithCorpus (UserName userMaster) (Left corpusMasterName) (Nothing :: Maybe HyperdataCorpus)
      masterListId <- getOrMkList masterCorpusId masterUserId
      _triggers <- initTriggers masterListId
      pure (masterUserId, masterRootId, masterCorpusId, masterListId)

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env createUsers
    _ <- runCmdDev env mkRoots
    x <- runCmdDev env initMaster
    putStrLn $ show x
    pure ()
