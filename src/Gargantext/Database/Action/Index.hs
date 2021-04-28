{-|
Module      : Gargantext.Database.Action.Index
Description : Indexation tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main Database functions for Gargantext.API.Node.Update

UpdateNodeParamsTexts { methodTexts :: Granularity }
data Granularity = NewNgrams | NewTexts | Both
    deriving (Generic, Eq, Ord, Enum, Bounded)

-- TODO add option for type of ngrams

-}

module Gargantext.Database.Action.Index
  where

import Data.List (nub)
import Gargantext.Core.Text.Terms.WithList (buildPatterns, filterTerms, termsInText)


index :: CorpusId -> Granularity -> Cmd err [Int]
index cId NewNgrams = do
  ngrams <- get ngrams with zero count
  texts  <- get all text to index
  indexSave text (buildPatterns ngrams)

index cId NewTexts = do
  ngrams <- get all ngrams
  texts  <- get text with zero count
  indexSave text (buildPatterns ngrams)

index cId Both = do
  r1 <- index cId NewNgrams
  r2 <- index cId NewTexts
  pure $ r1 <> r2

indexSave :: [Document] -> Pattern -> Cmd err [Int]
indexSave corpus p = do
  indexedDoc <- map (filterTerms patterns) corpus
  saveIndexDoc ngramsTextId






