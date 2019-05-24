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

import Data.Map         (Map,fromListWith)
import Data.Text        (Text)
import Data.Tuple       (fst)
import Data.Vector      (Vector)
import Gargantext.Prelude
import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Viz.Phylo
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Vector as Vector


-- | To init a list of Periods framed by a starting Date and an ending Date
initPeriods :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
initPeriods g s (start,end) = map (\l -> (head' "Doc" l, last' "Doc" l))
                            $ chunkAlong g s [start .. end]


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
parseDocs :: Vector Ngrams -> [(Date,Text)] -> [Document]
parseDocs roots c = map (\(d,t)
                -> Document d ( filter (\x -> Vector.elem x roots)
                              $ monoTexts t)) c

-- | To count the number of documents by year
countDocs :: [(Date,a)] -> Map Date Double
countDocs corpus = fromListWith (+) $ map (\(d,_) -> (d,1)) corpus




