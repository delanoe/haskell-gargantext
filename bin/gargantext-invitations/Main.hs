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

import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Node () -- instances only
import Gargantext.API.Prelude (GargError)
import Gargantext.Core.NLP (HasNLPServer)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (CmdRandom)
import Gargantext.Prelude
import Gargantext.Prelude.Config (readConfig)
import Prelude (read)
import System.Environment (getArgs)
import qualified Gargantext.API.Node.Share as Share

main :: IO ()
main = do
  params@[iniPath,user,node_id,email] <- getArgs

  _ <- if length params /= 4
      then panic "USAGE: ./gargantext-init gargantext.ini username node_id student@university.edu"
      else pure ()

  _cfg       <- readConfig         iniPath

  let invite :: (CmdRandom env GargError m, HasNLPServer env) => m Int
      invite = Share.api (UserName $ cs user) (NodeId $ (read node_id :: Int)) (Share.ShareTeamParams $ cs email)

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env invite
    pure ()
