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

module Gargantext.Viz.Phylo.Aggregates.Cluster
  where

import Data.List        (null,tail,concat,sort,intersect)
import Data.Map         (Map)
import Data.Tuple       (fst)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity
import Gargantext.Viz.Phylo.Metrics.Clustering
import Gargantext.Viz.Phylo.LinkMaker
import qualified Data.Map    as Map

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)


-- | Optimisation to filter only relevant candidates
getCandidates :: [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
getCandidates gs = filter (\(g,g') -> (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                 $ filter (\(g,g') -> g /= g')
                 $ listToDirectedCombi gs


-- | To transform a Graph into Clusters
graphToClusters :: Cluster -> GroupGraph -> [PhyloCluster]
graphToClusters clust (nodes,edges) = case clust of
      Louvain (LouvainParams _)      -> undefined
      RelatedComponents (RCParams _) -> relatedComp 0 (head' "graphToClusters" nodes) (tail nodes,edges) [] []
      _                              -> panic "[ERR][Viz.Phylo.Aggregates.Cluster.graphToClusters] not implemented"


-- | To transform a list of PhyloGroups into a Graph of Proximity
groupsToGraph :: Double -> Proximity -> [PhyloGroup] -> ([GroupNode],[GroupEdge])
groupsToGraph nbDocs prox gs = case prox of 
      WeightedLogJaccard (WLJParams _ sens) -> (gs, map (\(x,y) -> ((x,y), weightedLogJaccard sens (getGroupCooc x) (getGroupCooc y) nbDocs))
                                                  $ getCandidates gs)
      Hamming (HammingParams _)             -> (gs, map (\(x,y) -> ((x,y), hamming (getGroupCooc x) (getGroupCooc y))) $ getCandidates gs)
      _                                     -> undefined 


-- | To filter a Graph of Proximity using a given threshold
filterGraph :: Proximity -> ([GroupNode],[GroupEdge]) -> ([GroupNode],[GroupEdge])
filterGraph prox (ns,es) = case prox of
      WeightedLogJaccard (WLJParams thr _) -> (ns, filter (\(_,v) -> v >= thr) es)
      Hamming (HammingParams thr)          -> (ns, filter (\(_,v) -> v <= thr) es)
      _                                    -> undefined 


-- | To clusterise a Phylo
phyloToClusters :: Level -> Cluster -> Phylo -> Map (Date,Date) [PhyloCluster]
phyloToClusters lvl clus p = Map.fromList 
                            $ zip periods
                            $ map (\g -> if null (fst g)
                                         then []
                                         else graphToClusters clus g) graphs'
  where
    --------------------------------------
    graphs' :: [([GroupNode],[GroupEdge])]
    graphs' = traceGraphFiltered lvl
            $ map (\g -> filterGraph prox g) graphs
    --------------------------------------
    graphs  :: [([GroupNode],[GroupEdge])]
    graphs  = traceGraph lvl (getThreshold prox) 
            $ map (\prd -> groupsToGraph (periodsToNbDocs [prd] p) prox (getGroupsWithFilters lvl prd p))
            $ trace (show(map (\prd -> (prd,length $ getGroupsWithFilters lvl prd p)) periods)) periods 
    --------------------------------------
    prox :: Proximity
    prox = getProximity clus
    --------------------------------------
    periods :: [PhyloPeriodId]
    periods = getPhyloPeriods p
    --------------------------------------


----------------
-- | Tracer | --
----------------


traceGraph :: Level -> Double -> [([GroupNode],[GroupEdge])] -> [([GroupNode],[GroupEdge])]
traceGraph lvl thr g = trace ( "----\nUnfiltered clustering in Phylo" <> show (lvl) <> " :\n"
                                      <> "count : " <> show (length lst) <> " potential edges (" <> show (length $ filter (>= thr) lst) <> " >= " <> show (thr) <> ")\n"
                                      <> "similarity : " <> show (percentile 25 (VS.fromList lst)) <> " (25%) "
                                                         <> show (percentile 50 (VS.fromList lst)) <> " (50%) "
                                                         <> show (percentile 75 (VS.fromList lst)) <> " (75%) "
                                                         <> show (percentile 90 (VS.fromList lst)) <> " (90%)\n") g
  where 
    lst = sort $ map snd $ concat $ map snd g 


traceGraphFiltered :: Level -> [([GroupNode],[GroupEdge])] -> [([GroupNode],[GroupEdge])]
traceGraphFiltered lvl g = trace ( "----\nClustering in Phylo" <> show (lvl) <> " :\n"
                                      <> "count : " <> show (length lst) <> " edges\n"
                                      <> "similarity : " <> show (percentile 25 (VS.fromList lst)) <> " (25%) "
                                                         <> show (percentile 50 (VS.fromList lst)) <> " (50%) "
                                                         <> show (percentile 75 (VS.fromList lst)) <> " (75%) "
                                                         <> show (percentile 90 (VS.fromList lst)) <> " (90%)\n") g
  where 
    lst = sort $ map snd $ concat $ map snd g 


-- traceSim :: PhyloGroup -> PhyloGroup  -> Phylo -> Double -> Double
-- traceSim g g' p sim = trace (show (getGroupText g p) <> " [vs] " <>  show (getGroupText g' p) <> " = " <> show (sim) <> "\n") sim
