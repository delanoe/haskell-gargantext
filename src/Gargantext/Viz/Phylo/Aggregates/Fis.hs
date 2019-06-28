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

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (null,concat,sort,(++))
import Data.Map         (Map,elems,mapWithKey,unionWith,fromList,keys)
import Data.Tuple       (fst, snd)
import Data.Set         (size)
import Gargantext.Prelude
import Gargantext.Text.Metrics.FrequentItemSet  (fisWithSizePolyMap, Size(..))
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector.Storable as Vector

import Numeric.Statistics (percentile)

import Debug.Trace (trace)


-- | To apply a filter with the possibility of keeping some periods non empty (keep : True|False)
filterFis :: Bool -> Int -> (Int -> [PhyloFis] -> [PhyloFis]) -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFis keep thr f m = case keep of
  False -> Map.map (\l -> f thr l) m
  True  -> Map.map (\l -> keepFilled (f) thr l) m


-- | To filter Fis with small Support
filterFisBySupport :: Int -> [PhyloFis] -> [PhyloFis]
filterFisBySupport thr l = filter (\fis -> getSupport fis >= thr) l


-- | To filter Fis with small Clique size
filterFisByClique :: Int -> [PhyloFis] -> [PhyloFis]
filterFisByClique thr l = filter (\fis -> (size $ getClique fis) >= thr) l


-- | To filter nested Fis 
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head' "Fis" $ map getClique l) (map getClique l) []
                               in  filter (\fis -> elem (getClique fis) cliqueMax) l)


docsToFis' :: Map (Date,Date) [Document] -> Phylo -> Phylo
docsToFis' m p = if (null $ getPhyloFis p)
                 then trace("----\nRebuild the Fis from scratch\n") 
                    $ p & phylo_fis .~ mapWithKey (\k docs -> let fis = Map.toList $ fisWithSizePolyMap (Segment 1 20) 1 (map text docs)
                                                              in map (\f -> PhyloFis (fst f) (snd f) k) fis) m
                 else trace("----\nUse Fis from an existing file\n") 
                    $ p & phylo_fis %~ (unionWith (++) (fromList $ map (\k -> (k,[])) $ keys m))


toPhyloFis' :: Map (Date, Date) [PhyloFis] -> Bool -> Support -> Int -> Map (Date, Date) [PhyloFis]
toPhyloFis' fis k s t = traceFis "----\nFiltered Fis by clique size :\n"
                      $ filterFis k t (filterFisByClique)
                      $ traceFis "----\nFiltered Fis by nested :\n"
                      $ filterFisByNested 
                      $ traceFis "----\nFiltered Fis by support :\n"
                      $ filterFis k s (filterFisBySupport)
                      $ traceFis "----\nUnfiltered Fis :\n" fis                            


-----------------
-- | Tracers | --
-----------------



traceFis :: [Char] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis] 
traceFis lbl m = trace (lbl <> "count : " <> show (sum $ map length $ elems m) <> " Fis\n"
                            <> "support : " <> show (percentile 25 (Vector.fromList supps)) <> " (25%) "
                                            <> show (percentile 50 (Vector.fromList supps)) <> " (50%) "
                                            <> show (percentile 75 (Vector.fromList supps)) <> " (75%) "
                                            <> show (percentile 90 (Vector.fromList supps)) <> " (90%) "
                                            <> show (percentile 100 (Vector.fromList supps)) <> " (100%)\n"
                            <> "          " <> show (countSup 1 supps) <> " (>1) "
                                            <> show (countSup 2 supps) <> " (>2) "
                                            <> show (countSup 3 supps) <> " (>3) "
                                            <> show (countSup 4 supps) <> " (>4) "
                                            <> show (countSup 5 supps) <> " (>5) "
                                            <> show (countSup 6 supps) <> " (>6)\n"                                                                                                                                          
                            <> "clique size : " <> show (percentile 25 (Vector.fromList ngrms)) <> " (25%) "
                                                <> show (percentile 50 (Vector.fromList ngrms)) <> " (50%) "
                                                <> show (percentile 75 (Vector.fromList ngrms)) <> " (75%) "
                                                <> show (percentile 90 (Vector.fromList ngrms)) <> " (90%) "
                                                <> show (percentile 100 (Vector.fromList ngrms)) <> " (100%)\n"
                            <> "              " <> show (countSup 1 ngrms) <> " (>1) "
                                                <> show (countSup 2 ngrms) <> " (>2) "
                                                <> show (countSup 3 ngrms) <> " (>3) "
                                                <> show (countSup 4 ngrms) <> " (>4) "
                                                <> show (countSup 5 ngrms) <> " (>5) "
                                                <> show (countSup 6 ngrms) <> " (>6)\n"                                                                                             
                            ) m
  where
    --------------------------------------
    countSup :: Double -> [Double] -> Int
    countSup s l = length $ filter (>s) l 
    --------------------------------------
    supps :: [Double]
    supps = sort $ map (fromIntegral . _phyloFis_support) $ concat $ elems m
    --------------------------------------
    ngrms :: [Double]
    ngrms = sort $ map (\f -> fromIntegral $ Set.size $ _phyloFis_clique f) $ concat $ elems m
    --------------------------------------