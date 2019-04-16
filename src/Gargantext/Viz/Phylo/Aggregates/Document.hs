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

import Control.Lens         hiding (both, Level)

import Data.List        (last,nub,(++))
import Data.Map         (Map,member)
import Data.Text        (Text)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)
import Gargantext.Prelude
import Gargantext.Text.Terms.Mono               (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Vector as Vector


-- | To init a list of Periods framed by a starting Date and an ending Date
initPeriods :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
initPeriods g s (start,end) = map (\l -> (head' "Doc" l, last l))
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

-- | Reduce a list of foundations as a list of corresponding roots 
reduceByRoots :: Map Ngrams Ngrams -> [Ngrams] -> [Ngrams]
reduceByRoots m ns = (\(f,s) -> f ++ (nub s))
                    $ foldl (\mem n -> if member n m
                                      then (fst mem,(snd mem) ++ [m Map.! n])
                                      else ((fst mem) ++ [n],snd mem)
                                      ) ([],[]) ns

-- | To parse a list of Documents by filtering on a Vector of Ngrams
parseDocs :: Vector Ngrams -> PhyloRoots -> [(Date,Text)] -> [Document]
parseDocs fds roots c = map (\(d,t)
                         -> Document d ( reduceByRoots mRoots
                                       $ filter (\x -> Vector.elem x fds)
                                       $ monoTexts t)) c
  where
    --------------------------------------
    mRoots :: Map Ngrams Ngrams
    mRoots = forestToMap (roots ^. phylo_rootsForest)
    --------------------------------------


-- | To transform a Corpus of texts into a Map of aggregated Documents grouped by Periods


