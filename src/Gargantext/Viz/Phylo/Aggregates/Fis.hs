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

import Data.List        (last,head,null)
import Data.Map         (Map, empty)
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
filterMinorFis min l = filter (\fis -> getSupport fis > min) l


-- | To filter nested Fis 
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head $ map getClique l) (map getClique l) []
                               in  filter (\fis -> elem (getClique fis) cliqueMax) l)


-- | To transform a list of Documents into a Frequent Items Set 
docsToFis :: Map (Date, Date) [Document] -> Map (Date, Date) [PhyloFis]
docsToFis docs = map (\d -> let fs = Map.toList $ fisWithSizePolyMap (Segment 1 20) 1 (map text d)
                            in map (\f -> PhyloFis (fst f) (snd f) empty) fs) docs


-- | To process a list of Filters on top of the PhyloFis
processFilters :: [Filter] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
processFilters filters phyloFis
  | null filters = phyloFis
  | otherwise    = panic "[ERR][Viz.Phylo.LevelMaker.processFilters] please add some filters for the Fis"


-- | To process a list of Metrics on top of the PhyloFis
processMetrics :: [Metric] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
processMetrics metrics phyloFis
  | null metrics = phyloFis
  | otherwise    = panic "[ERR][Viz.Phylo.LevelMaker.processMetrics] please add some metrics for the Fis"


-- | To transform some Documents into PhyloFis and apply a List of Metrics and Filters
toPhyloFis :: Map (Date, Date) [Document] -> Bool -> Support -> [Metric] -> [Filter] -> Map (Date, Date) [PhyloFis]
toPhyloFis ds k s ms fs = processFilters fs  
                        $ processMetrics ms
                        $ filterFisByNested 
                        $ filterFisBySupport k s
                        $ docsToFis ds  