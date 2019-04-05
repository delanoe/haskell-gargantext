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

module Gargantext.Viz.Phylo.Metrics.Clustering
  where

import Data.Graph.Clustering.Louvain.CplusPlus
import Data.List        (last,head,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!))
import Data.Map         (fromList,mapKeys)
import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

-- | To apply the related components method to a PhyloGraph
-- curr = the current PhyloGroup 
-- (nodes,edges) = the initial PhyloGraph minus the current PhyloGroup
-- next = the next PhyloGroups to be added in the cluster  
-- memo = the memory of the allready created clusters
relatedComp :: Int -> PhyloGroup -> PhyloGraph -> [PhyloGroup] -> [[PhyloGroup]] -> [[PhyloGroup]]
relatedComp idx curr (nodes,edges) next memo
  | null nodes' && null next' = memo'
  | (not . null) next'        = relatedComp idx (head next') (nodes',edges) (tail next') memo'
  | otherwise                 = relatedComp (idx + 1) (head nodes') (tail nodes',edges) [] memo'
  where
    --------------------------------------
    memo' :: [[PhyloGroup]]
    memo' 
      | null memo                  = [[curr]]
      | idx == ((length memo) - 1) = (init memo) ++ [(last memo) ++ [curr]]
      | otherwise                  = memo ++ [[curr]]
    --------------------------------------
    next' :: [PhyloGroup]
    next' = filter (\x -> not $ elem x $ concat memo) $ nub $ next ++ (getNeighbours False curr edges)
    --------------------------------------
    nodes' :: [PhyloGroup]
    nodes' = filter (\x -> not $ elem x next') nodes
    --------------------------------------


louvain :: (PhyloNodes,PhyloEdges) -> IO [[PhyloGroup]]
louvain (nodes,edges) = map (\community -> map (\node -> nodes !! (l_node_id node)) community)
                      <$> groupBy (\a b -> (l_community_id a) == (l_community_id b))
                      <$> (cLouvain $ mapKeys (\(x,y) -> (idx x, idx y)) $ fromList edges)
  where
    -------------------------------------- 
    idx :: PhyloGroup -> Int
    idx e = case elemIndex e nodes of
      Nothing -> panic "[ERR][Gargantext.Viz.Phylo.Metrics.Clustering] a node is missing"
      Just i  -> i
    --------------------------------------  
