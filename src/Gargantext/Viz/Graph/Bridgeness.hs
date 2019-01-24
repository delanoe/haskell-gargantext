{-|
Module      : Gargantext.Viz.Graph.Bridgeness
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Viz.Graph.Bridgeness (bridgeness)
  where

import Gargantext.Prelude
--import Gargantext.Viz.Graph
import Data.Map (Map, fromListWith, lookup, fromList, keys)
import Data.Maybe (catMaybes)
import Data.List (sortOn, concat)
import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))

type Bridgeness = Double

-- TODO mv in Louvain Lib
type LouvainNodeId = Int
type CommunityId   = Int

partition2map :: [LouvainNode] -> Map CommunityId [LouvainNodeId]
partition2map ns = fromListWith (<>) [ (cId, [nId]) | LouvainNode nId cId <- ns]

ordEdgesBetween :: (Ord distance, Ord node)
                => [node] -> [node]
                -> Map (node, node) distance
                -> [((node, node), distance)]
ordEdgesBetween c1 c2 d = sortOn snd $ catMaybes
              [ (,) <$> Just   (n1,n2)
                    <*> lookup (n1,n2) d
              | n1 <- c1
              , n2 <- c2
              , n1 < n2
              ]

filterEdgesBetween :: (RealFrac b, Ord node, Ord distance) =>
     b -> [node] -> [node]
     -> Map (node, node) distance
     -> [((node, node), distance)]
filterEdgesBetween b c1 c2 d = take n d'
  where
    n  = round $ b * i / (len c1 + len c2)
    d' = ordEdgesBetween c1 c2 d
    i  = fromIntegral $ length d'
    len c = fromIntegral $ length (ordEdgesBetween c c d)


bridgeness :: Bridgeness
           -> [LouvainNode]
           -> Map (LouvainNodeId, LouvainNodeId) Double
           -> Map (LouvainNodeId, LouvainNodeId) Double
bridgeness b ns' ds = fromList . concat . map (\(c1,c2) -> filterEdgesBetween b c1 c2 ds) $ p
  where
    p  = catMaybes [ (,) <$> lookup k1 ns <*> lookup k2 ns | k1 <- ks, k2 <- ks, k1 < k2]
    ns = partition2map ns'
    ks = keys ns



