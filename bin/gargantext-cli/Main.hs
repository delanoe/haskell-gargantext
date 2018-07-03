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

import Data.Text (Text)
import System.Environment
--import Control.Concurrent.Async as CCA (mapConcurrently)

import Gargantext.Prelude
import Gargantext.Text.Context
import Gargantext.Text.Terms
import Gargantext.Text.Terms.WithList
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms (terms)
import Gargantext.Text.Metrics.Count (cooc)

main :: IO ()
main = do
  [corpusFile, termListFile, outputFile] <- getArgs

  -- corpus :: [Text]
  corpus <- DV.toList <$> map (\n -> (csv_title n) <> " " <> (csv_abstract n))
                      <$> snd
                      <$> readCsv corpusFile
  
  putStrLn $ show $ length corpus
  -- termListMap :: [Text]
  termList <- csvGraphTermList termListFile
  
  putStrLn $ show $ length termList

  let patterns = WithList $ buildPatterns termList
  corpusIndexed <- mapM (terms patterns) corpus
  
  putStrLn $ show corpusIndexed
  let myCooc = cooc corpusIndexed

  putStrLn $ show myCooc
  --writeFile outputFile cooc
