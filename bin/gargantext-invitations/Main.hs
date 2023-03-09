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
import Prelude (getLine, read)
import System.Environment (getArgs)
import Gargantext.Database.Action.User.New (newUsers)
import Gargantext.Core.Types.Individu (User(..))
import qualified Gargantext.API.Node.Share as Share

main :: IO ()
main = do
  params@[iniPath,user,node_id,email] <- getArgs

  _ <- if length params /= 4
      then panic "USAGE: ./gargantext-init gargantext.ini username node_id student@university.edu"
      else pure ()

  cfg       <- readConfig         iniPath

  let invite :: CmdR GargError Int
      invite = Share.api (UserName $ cs user) (NodeId $ (read node_id :: Int)) (Share.ShareTeamParams $ cs email)

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env invite
    pure ()
