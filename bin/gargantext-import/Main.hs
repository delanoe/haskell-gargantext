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
{-# LANGUAGE Strict            #-}

module Main where

import Servant (ServantErr)
import Gargantext.Prelude
import Gargantext.Database.Flow (flowCorpus)
import Gargantext.Text.Parsers (FileFormat(CsvHalFormat))
import Gargantext.Database.Utils (Cmd, connectGargandb, runCmdDevWith)
import Gargantext.Database.Types.Node (NodeId)
--import Gargantext.Database.Schema.User (insertUsers, gargantuaUser, simpleUser)
import Gargantext.API.Node () -- instances
import Gargantext.API.Ngrams (RepoCmdM)
import System.Environment (getArgs)

main :: IO ()
main = do
  [iniPath, name, corpusPath] <- getArgs

  {-let createUsers :: Cmd ServantErr Int64
      createUsers = insertUsers [gargantuaUser,simpleUser]
  _ <- runCmdDevWith iniPath createUsers
  -}

  {- -- TODO missing repo var...
  let cmd :: RepoCmdM env ServantErr m => m NodeId
      cmd = flowCorpus CsvHalFormat corpusPath (cs name)
  r <- runCmdDevWith iniPath cmd
  -}
  pure ()


