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

{-# LANGUAGE Strict            #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Text (Text)
import Gargantext.API.Admin.EnvTypes (DevEnv)
import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Prelude (GargError)
import Gargantext.API.Node () -- instances only
-- import Gargantext.API.Ngrams.Tools (getRepo)
import Gargantext.Database.Query.Table.Node (getOrMkList)
import Data.Either (Either(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataCorpus)
import Gargantext.Database.Prelude (Cmd'', )
import Gargantext.Database.Action.Flow (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Individu (User(..), arbitraryNewUsers, NewUser(..), arbitraryUsername, GargPassword(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import System.Environment (getArgs)
import Prelude (getLine)
import GHC.IO.Exception (IOException)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Database.Prelude (Cmd, execPGSQuery)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)

import Gargantext.Database.Admin.Trigger.Init
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)



main :: IO ()
main = do

  putStrLn "Manual method:"
  putStrLn "Upgrade your GarganText instance with the script:"
  putStrLn "./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.5/schema.sql"
  putStrLn "Then press enter key when you are done."
  _ok  <- getLine
  [iniPath] <- getArgs
  cfg       <- readConfig         iniPath
  let secret = _gc_secretkey cfg


  let
    sqlUpdateTriggerHash :: Cmd'' DevEnv IOException Int64
    sqlUpdateTriggerHash = do
      execPGSQuery query ()
        where
          query = [sql|
          UPDATE nodes SET typename = typename;
         |]

  let
    sqlNodes2Context :: Cmd'' DevEnv IOException Int64
    sqlNodes2Context = do
      execPGSQuery query ()
        where
          query = [sql|
          UPDATE nodes SET typename = typename;
         |]


  let
    contextsTriggers :: Cmd GargError ()
    contextsTriggers = do
      (masterUserId, _masterRootId, masterCorpusId)
                  <- getOrMk_RootWithCorpus (UserName userMaster)
                                            (Left corpusMasterName)
                                            (Nothing :: Maybe HyperdataCorpus)
      masterListId <- getOrMkList masterCorpusId masterUserId
      _triggers    <- initLastTriggers masterListId
      pure ()


  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env (initFirstTriggers secret :: Cmd GargError [Int64])
    _ <- runCmdDev env (contextsTriggers         :: Cmd GargError ())
    _ <- runCmdDev env sqlUpdateTriggerHash
    putStrLn "Uprade done with success"
    pure ()
