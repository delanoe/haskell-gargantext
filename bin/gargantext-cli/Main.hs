{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main specifications to index a corpus with a term list

 -}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE Strict             #-}

module Main where

import qualified Data.Vector as DV

import Gargantext.Prelude
import Data.Text (Text)
import System.Environment

import Gargantext.Text.Parsers.CSV (readCsv, csv_abstract)
import Gargantext.Text.List.CSV (fromCsvListFile)

main :: IO ()
main = do
  [corpusFile, termListFile, outputFile] <- getArgs

  -- corpus :: [Text]
  corpus   <- DV.toList . fmap csv_abstract . snd <$> readCsv corpusFile

  -- termListMap :: [Text]
  termList <- termListMap <$> fromCsvListFile termListFile

  let corpusIndexed = indexCorpusWith corpus termList
  let cooc = cooccurrences corpusIndexed

  writeFile outputFile cooc
