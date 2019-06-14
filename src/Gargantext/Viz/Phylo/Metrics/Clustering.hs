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
import Data.List        (concat,null,nub,(++),elemIndex,groupBy,(!!), (\\), union, intersect)
import Data.Map         (fromList,mapKeys)
import Gargantext.Prelude
import Gargantext.Viz.Phylo

relatedComp :: [[PhyloGroup]] -> [[PhyloGroup]]
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
                      <$> (cLouvain $ mapKeys (\(x,y) -> (idx x, idx y)) $ fromList edges)
  where
    -------------------------------------- 
    idx :: PhyloGroup -> Int
    idx e = case elemIndex e nodes of
      Nothing -> panic "[ERR][Gargantext.Viz.Phylo.Metrics.Clustering] a node is missing"
      Just i  -> i
    --------------------------------------  
