{-|
Module      : Main.hs
Description : GarganText Mailing Invitations
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Strict            #-}

module Main where

import Data.Either (Either(..))
import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Node () -- instances only
import Gargantext.API.Prelude (GargError)
import Gargantext.Core.Types.Individu (User(..), arbitraryNewUsers, NewUser(..), arbitraryUsername, GargPassword(..))
import Gargantext.Database.Action.Flow (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Trigger.Init (initFirstTriggers, initLastTriggers)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataCorpus)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (CmdR)
import Gargantext.Database.Query.Table.Node (getOrMkList)
import Gargantext.Database.Query.Table.User (insertNewUsers, )
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import Prelude (getLine)
import System.Environment (getArgs)
import Gargantext.Database.Action.User.New (newUsers)

main :: IO ()
main = do
  params@[iniPath,email] <- getArgs

  _ <- if length params /= 2
      then panic "USAGE: ./gargantext-init gargantext.ini student@university.edu"
      else pure ()

  cfg       <- readConfig         iniPath

  let createUsers :: CmdR GargError Int64
      createUsers = newUsers [cs email]

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env createUsers
    pure ()
