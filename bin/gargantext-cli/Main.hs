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

import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.List (cycle)
import System.IO (hPutStr, hFlush, stderr)
import System.Environment
import Control.Concurrent.Async as CCA (mapConcurrently)

import Gargantext.Prelude
import Gargantext.Text.Context
import Gargantext.Text.Terms
import Gargantext.Text.Terms.WithList
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms (terms)
import Gargantext.Text.Metrics.Count (coocOn)

mapMP :: MonadIO m => (a -> m b) -> [a] -> m [b]
mapMP f xs = do
    bs <- zipWithM g (cycle "-\\|/") xs
    liftIO $ hPutStr stderr "\rDone\n"
    pure bs
  where
    g c x = do
      liftIO $ hPutStr stderr ['\r',c]
      liftIO $ hFlush  stderr
      f x

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
  --corpusIndexed <- mapMP (terms patterns) corpus
  corpusIndexed <- mapConcurrently (terms patterns) corpus
  mapM (putStrLn . show) corpusIndexed
  let myCooc = coocOn identity corpusIndexed

  putStrLn $ show myCooc
  --writeFile outputFile cooc
