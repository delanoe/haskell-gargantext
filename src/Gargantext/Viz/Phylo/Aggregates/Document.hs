{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.Aggregates.Document
  where

import Data.List        (last,head)
import Data.Map         (Map)
import Data.Text        (Text, unwords)
import Data.Tuple       (fst)
import Data.Vector      (Vector)
import Gargantext.Prelude                       hiding (head)
import Gargantext.Text.Terms.Mono               (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Vector as Vector


-- | To init a list of Periods framed by a starting Date and an ending Date
initPeriods :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
initPeriods g s (start,end) = map (\l -> (head l, last l))
                              $ chunkAlong g s [start .. end]


-- | To be defined, for the moment it's just the id function
groupNgramsWithTrees :: Ngrams -> Ngrams
groupNgramsWithTrees n = n


-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: (Ord date, Enum date) => (doc -> date) -> [(date,date)] -> [doc] -> Map (date, date) [doc]
groupDocsByPeriod _ _   [] = panic "[ERR][Viz.Phylo.Example.docsToPeriods] Empty [Documents] can not have any periods"
groupDocsByPeriod f pds es = Map.fromList $ zip pds $ map (inPeriode f es) pds
  where
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ List.partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------


-- | To parse a list of Documents by filtering on a Vector of Ngrams
parseDocs :: (Ngrams -> Ngrams) -> Vector Ngrams -> [Document] -> [Document]
parseDocs f l docs = map (\(Document d t)
                         -> Document d ( unwords
                         -- | To do : change 'f' for the Ngrams Tree Agregation
                                      $ map f
                                      $ filter (\x -> Vector.elem x l)
                                      $ monoTexts t)) docs


-- | To transform a Corpus of texts into a Map of aggregated Documents grouped by Periods
corpusToDocs :: (Ngrams -> Ngrams) -> [(Date,Text)] -> Phylo -> Map (Date,Date) [Document]
corpusToDocs f c p = groupDocsByPeriod date (getPhyloPeriods p)
                   $ parseDocs f (getFoundations p) docs
  where
    --------------------------------------
    docs :: [Document]
    docs = map (\(d,t) -> Document d t) c
    --------------------------------------
