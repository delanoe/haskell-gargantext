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

import Data.List        (null,concat,sort)
import Data.Map         (Map, empty,elems)
import Data.Tuple       (fst, snd)
import Data.Set         (size)
import Data.Vector.Storable  (Vector)
import Gargantext.Prelude
import Gargantext.Text.Metrics.FrequentItemSet  (fisWithSizePolyMap, Size(..))
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector.Storable as Vector

import Numeric.Statistics (percentile)

import Debug.Trace (trace)


-- | To Filter Fis by support 
filterFisBySupport :: Bool -> Int -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisBySupport keep min' m = case keep of
  False -> Map.map (\l -> filterMinorFis min' l) m
  True  -> Map.map (\l -> keepFilled (filterMinorFis) min' l) m


filterFisByNgrams :: Int -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNgrams thr m = Map.map(\lst -> filter (\fis -> (size $ getClique fis) > thr) lst) m


-- | To filter Fis with small Support, to preserve nonempty periods please use : filterFisBySupport true
filterMinorFis :: Int -> [PhyloFis] -> [PhyloFis]
filterMinorFis min' l = filter (\fis -> getSupport fis > min') l


-- | To filter nested Fis 
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head' "Fis" $ map getClique l) (map getClique l) []
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
toPhyloFis :: Map (Date, Date) [Document] -> Bool -> Support -> Int -> [Metric] -> [Filter] -> Map (Date, Date) [PhyloFis]
toPhyloFis ds k s t ms fs = processFilters fs  
                          $ processMetrics ms
                          $ traceFis "----\nFiltered Fis by clique size :\n"
                          $ filterFisByNgrams t
                          $ traceFis "----\nFiltered Fis by nested :\n"
                          $ filterFisByNested 
                          $ traceFis "----\nFiltered Fis by support :\n"
                          $ filterFisBySupport k s
                          $ traceFis "----\nUnfiltered Fis :\n"
                          $ docsToFis ds  


-----------------
-- | Tracers | --
-----------------

traceFis :: [Char] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis] 
traceFis lbl m = trace (lbl <> "count : " <> show (sum $ map length $ elems m) <> " Fis\n"
                            <> "support : " <> show (percentile 25 supps) <> " (25%) "
                                            <> show (percentile 50 supps) <> " (50%) "
                                            <> show (percentile 75 supps) <> " (75%) "
                                            <> show (percentile 90 supps) <> " (90%)\n"
                            <> "clique size : " <> show (percentile 25 ngrms) <> " (25%) "
                                                <> show (percentile 50 ngrms) <> " (50%) "
                                                <> show (percentile 75 ngrms) <> " (75%) "
                                                <> show (percentile 90 ngrms) <> " (90%)\n"                                             
                            ) m
  where 
    supps :: Vector Double
    supps = Vector.fromList $ sort $ map (fromIntegral . _phyloFis_support) $ concat $ elems m
    ngrms :: Vector Double
    ngrms = Vector.fromList $ sort $ map (\f -> fromIntegral $ Set.size $ _phyloFis_clique f) $ concat $ elems m