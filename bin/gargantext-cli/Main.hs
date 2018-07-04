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
import qualified Data.Maybe  as DMaybe

import Control.Monad (zipWithM)
import Control.Monad.IO.Class

import qualified Data.IntMap as DM

import Data.Map (Map)
import Data.Text (Text)
import Data.List (cycle, concat)
import Data.List.Split (chunksOf)
import System.IO (hPutStr, hFlush, stderr)
import System.Environment
import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Concurrent (getNumCapabilities)
import Prelude ((>>))

import Gargantext.Prelude
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Text.Terms
import Gargantext.Text.Terms.WithList
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms (terms)
import Gargantext.Text.Metrics.Count (coocOn, Coocs)

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

mapConcurrentlyChunked :: (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyChunked f ts = do
  n <- getNumCapabilities
  concat <$> mapConcurrently (mapM f) (chunksOf n ts)

filterTermsAndCooc
  :: TermType Lang
     -> (Int, [Text])
     -> IO (Map (Terms, Terms) Coocs)
filterTermsAndCooc patterns (year, ts) = do
  log "start"
  r <- coocOn identity <$> mapM (\x -> {-log "work" >>-} terms patterns x) ts
  log "stop"
  pure r
  where
    log m = putStrLn $ "filterTermsAndCooc: " <> m <> " " <> show year

--main :: IO [()]
main = do
  [corpusFile, termListFile, _] <- getArgs

  --corpus :: IO (DM.IntMap [[Text]])
  corpus <- DM.fromListWith (<>)
                             . DV.toList
                             . DV.map (\n -> (csv_publication_year n, [(csv_title n) <> " " <> (csv_abstract n)]))
                             . snd
                           <$> readCsv corpusFile

  -- termListMap :: [Text]
  termList <- csvGraphTermList termListFile

  putStrLn $ show $ length termList

  let years = DM.keys corpus
  let patterns = WithList $ buildPatterns termList
  let corpus' = DMaybe.catMaybes $ map (\k -> DM.lookup k corpus) years


  r <- mapConcurrentlyChunked (filterTermsAndCooc patterns) (zip years corpus')
  putStrLn $ show r
  --writeFile outputFile cooc
