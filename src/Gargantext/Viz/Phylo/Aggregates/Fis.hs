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

module Gargantext.Viz.Phylo.Aggregates.Fis
  where

import Data.List        (last,head)
import Data.Map         (Map)
import Data.Text        (Text, unwords, toLower, words)
import Data.Tuple       (fst, snd)
import Data.Tuple.Extra
import Data.Vector      (Vector)

import Gargantext.Prelude                       hiding (head)
import Gargantext.Text.Metrics.FrequentItemSet  (fisWithSizePolyMap, Size(..))
import Gargantext.Text.Terms.Mono               (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Vector as Vector 


-- | To Filter Fis by support 
filterFisBySupport :: Bool -> Int -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisBySupport keep min m = case keep of
  False -> Map.map (\l -> filterMinorFis min l) m
  True  -> Map.map (\l -> keepFilled (filterMinorFis) min l) m


-- | To filter Fis with small Support, to preserve nonempty periods please use : filterFisBySupport true
filterMinorFis :: Int -> [PhyloFis] -> [PhyloFis]
filterMinorFis min l = filter (\fis -> snd fis > min) l


-- | To filter nested Fis 
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head $ map fst l) (map fst l) []
                               in  filter (\fis -> elem (fst fis) cliqueMax) l)


-- | To transform a list of Documents into a Frequent Items Set 
docsToFis :: Map (Date, Date) [Document] -> Map (Date, Date) [PhyloFis]
docsToFis docs = map (\d -> Map.toList 
                          $ fisWithSizePolyMap (Segment 1 20) 1 (map (words . text) d)) docs