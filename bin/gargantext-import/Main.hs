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
{-# LANGUAGE Strict             #-}

module Main where

import Servant (ServantErr)
import Gargantext.Prelude
import Gargantext.Database.Flow (flowCorpus)
import Gargantext.Text.Parsers (FileFormat(CsvHalFormat))
import Gargantext.Database.Utils (connectGargandb, runCmdDevWith)
import System.Environment (getArgs)

-- main :: IO ()
main = do
  [iniPath, name, corpusPath] <- getArgs
  
  r <- runCmdDevWith iniPath $ flowCorpus CsvHalFormat corpusPath (cs name) :: Cmd ServantErr NodeId
  pure ()


