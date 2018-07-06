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

import Data.Maybe (catMaybes)
import Data.Text (pack)
import qualified Data.Vector as DV
import qualified Data.Maybe  as DMaybe

import Control.Monad (zipWithM)
import Control.Monad.IO.Class

import qualified Data.IntMap as DM

import Data.Map (Map)
import Data.Text (Text)
import Data.List (cycle, concat, unwords)
import Data.List.Split (chunksOf)
import System.IO (hPutStr, hFlush, stderr)
import System.Environment
import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import Prelude ((>>))

import Gargantext.Prelude
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Text.Terms
import Gargantext.Text.Context
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
  caps <- getNumCapabilities
  let n = 1 `max` (length ts `div` caps)
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
    log m = do
      tid    <- myThreadId
      (p, _) <- threadCapability tid
      putStrLn . unwords $
        ["filterTermsAndCooc:", m, show year, "on proc", show p]

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

  let patterns = WithList $ buildPatterns termList

  -- r <- mapConcurrentlyChunked (filterTermsAndCooc patterns) (DM.toList corpus)
  r <- mapConcurrently (filterTermsAndCooc patterns) (DM.toList corpus)
  putStrLn $ show r
  --writeFile outputFile cooc


testCooc = do
  let patterns = WithList $ buildPatterns testTermList
  mapM (\x -> {-log "work" >>-} terms patterns x) $ catMaybes $ map (head . snd) testCorpus
  --mapConcurrently (filterTermsAndCooc patterns) testCorpus


testCorpus :: [(Int, [Text])]
testCorpus = [ (1998, [pack "The beees"])
             , (1999, [ pack "The bees and the flowers" 
                      --, pack "The bees and the flowers" 
                      ])
             ]

testTermList :: TermList
testTermList = [ ([pack "bee"], [[pack "bees"]])
               , ([pack "flower"], [[pack "flowers"]])
               ]

