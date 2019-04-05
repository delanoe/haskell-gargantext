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

import Data.List        (head)
import Data.Map         (Map)
import Data.Text        (words)
import Data.Tuple       (fst, snd)
import Gargantext.Prelude                       hiding (head)
import Gargantext.Text.Metrics.FrequentItemSet  (fisWithSizePolyMap, Size(..))
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.Map    as Map


-- | To Filter Fis by support
filterFisBySupport :: Bool -> Int -> Map (Date, Date) [Fis] -> Map (Date, Date) [Fis]
filterFisBySupport empty min' m = case empty of
  True  -> Map.map (\l -> filterMinorFis min' l) m
  False -> Map.map (\l -> keepFilled (filterMinorFis) min' l) m


-- | To filter Fis with small Support, to preserve nonempty periods please use : filterFisBySupport False
filterMinorFis :: Int -> [Fis] -> [Fis]
filterMinorFis min' l = filter (\fis -> snd fis > min') l


-- | To filter nested Fis
filterFisByNested :: Map (Date, Date) [Fis] -> Map (Date, Date) [Fis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head $ map fst l) (map fst l) []
                               in  filter (\fis -> elem (fst fis) cliqueMax) l)


-- | To transform a list of Documents into a Frequent Items Set
docsToFis :: Map (Date, Date) [Document] -> Map (Date, Date) [Fis]
docsToFis docs = map (\d -> Map.toList
                          $ fisWithSizePolyMap (Segment 1 20) 1 (map (words . text) d)) docs
