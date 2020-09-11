{-|
Module      : Gargantext.Core.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE TemplateHaskell #-}

module Gargantext.Core.Viz.Phylo.Cluster
  where
import Control.Parallel.Strategies
import Data.Graph.Clustering.Louvain.CplusPlus
import Data.Graph.Clustering.Louvain.Utils (LouvainNode(..))
import Data.List        (null,concat,sort,intersect,(++), elemIndex, groupBy, nub, union, (\\), (!!))
import Data.Map         (Map, fromList, mapKeys)
import Data.Tuple       (fst)
import Gargantext.Prelude
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.Tools
import Gargantext.Core.Viz.Phylo.Metrics
import Gargantext.Core.Viz.Phylo.LinkMaker
import qualified Data.Map    as Map

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)


--------------
-- | Algo | --
--------------


relatedComp :: Eq a => [[a]] -> [[a]]
relatedComp graphs = foldl' (\mem groups -> 
  if (null mem)
  then mem ++ [groups]
  else 
    let related = filter (\groups' -> (not . null) $ intersect groups groups') mem
    in if (null related)
       then mem ++ [groups]
       else (mem \\ related) ++ [union groups (nub $ concat related)] ) [] graphs


louvain :: ([GroupNode],[GroupEdge]) -> IO [[PhyloGroup]]
louvain (nodes,edges) = map (\community -> map (\node -> nodes !! (l_node_id node)) community)
                      <$> groupBy (\a b -> (l_community_id a) == (l_community_id b))
                      <$> (cLouvain "0.0001" $ mapKeys (\(x,y) -> (idx x, idx y)) $ fromList edges)
  where
    -------------------------------------- 
    idx :: PhyloGroup -> Int
    idx e = case elemIndex e nodes of
      Nothing -> panic "[ERR][Gargantext.Core.Viz.Phylo.Metrics.Clustering] a node is missing"
      Just i  -> i
    --------------------------------------  


-----------------------
-- | Cluster Maker | --
-----------------------


-- | Optimisation to filter only relevant candidates
getCandidates :: [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
getCandidates gs = filter (\(g,g') -> (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                 $ filter (\(g,g') -> g /= g')
                 $ listToDirectedCombi gs


-- | To transform a Graph into Clusters
graphToClusters :: Cluster -> GroupGraph -> [PhyloCluster]
graphToClusters clust (nodes,edges) = case clust of
      Louvain (LouvainParams _)      -> undefined
      RelatedComponents (RCParams _) -> relatedComp $ ((map (\((g,g'),_) -> [g,g']) edges) ++ (map (\g -> [g]) nodes))
      _                              -> panic "[ERR][Viz.Phylo.Aggregates.Cluster.graphToClusters] not implemented"


-- | To transform a list of PhyloGroups into a Graph of Proximity
groupsToGraph :: Double -> Proximity -> [PhyloGroup] -> ([GroupNode],[GroupEdge])
groupsToGraph nbDocs prox gs = case prox of 
      WeightedLogJaccard (WLJParams _ sens) -> (gs, let candidates  = map (\(x,y) -> ((x,y), weightedLogJaccard sens nbDocs (getGroupCooc x) (getGroupCooc y) (getGroupNgrams x) (getGroupNgrams y)))
                                                                    $ getCandidates gs
                                                        candidates' = candidates `using` parList rdeepseq
                                                    in  candidates' )
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
    graphs :: [([GroupNode],[GroupEdge])]
    graphs = traceGraph lvl (getThreshold prox)
           $ let gs  = (trace $ "PROX: " <> show prox) $ map (\prd -> groupsToGraph (periodsToNbDocs [prd] p) prox (getGroupsWithFilters lvl prd p)) periods
                 gs' = gs `using` parList rdeepseq
             in  gs'
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
