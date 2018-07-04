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

import qualified Data.Map.Strict as DM

import Data.Map (Map)
import Data.Text (Text)
import Data.List (cycle)
import System.IO (hPutStr, hFlush, stderr)
import System.Environment
import Control.Concurrent.Async as CCA (mapConcurrently)

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




filterTermsAndCooc
  :: TermType Lang
     -> [Text]
     -> IO (Map (Terms, Terms) Coocs)
filterTermsAndCooc patterns ts = coocOn identity <$> mapM (terms patterns) ts


--main :: IO [()]
main = do
  [corpusFile, termListFile, _] <- getArgs

  -- corpus :: [Text]
  corpus <- foldl' (\m e -> DM.insertWith (\_ x -> (snd e) <> x) (fst e) [] m) DM.empty
                           <$> DV.toList
                           <$> DV.map (\n -> (csv_publication_year n, [(csv_title n) <> " " <> (csv_abstract n)]))
                           <$> snd
                           <$> readCsv corpusFile
  
  -- termListMap :: [Text]
  termList <- csvGraphTermList termListFile
  
  putStrLn $ show $ length termList
  
  let years = DM.keys corpus
  let patterns = WithList $ buildPatterns termList
  let corpus' = DMaybe.catMaybes $ map (\k -> DM.lookup k corpus) years


  r <- zip years <$> mapConcurrently (filterTermsAndCooc patterns) corpus'
  putStrLn $ show r
  --writeFile outputFile cooc
