{-|
Module      : Gargantext.Viz.Graph.Bridgeness
Description : Bridgeness filter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Let be a graph with partitions (from Louvain algo), Bridgeness uniformly
filters inter-communities links.

TODO rewrite Bridgeness with "equivalence structurale" metrics (Confluence)

TODO use Map LouvainNodeId (Map LouvainNodeId)





-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Viz.Graph.Bridgeness (bridgeness)
  where

import Data.Ord (Down(..))
import Gargantext.Prelude
import Data.Map (Map, fromListWith, lookup, fromList, toList, mapWithKey, elems)
import qualified Data.Map as DM
import Data.Maybe (catMaybes)
import Data.List (concat, sortOn)
import Data.Graph.Clustering.Louvain.Utils (LouvainNode(..))


-- TODO mv in Louvain Lib
type LouvainNodeId = Int
type CommunityId   = Int

type Bridgeness = Double


bridgeness :: Bridgeness
           -> [LouvainNode]
           -> Map (LouvainNodeId, LouvainNodeId) Double
           -> Map (LouvainNodeId, LouvainNodeId) Double
bridgeness b ns = DM.fromList
                . concat
                . DM.elems
                . filterComs b
                . groupEdges (nodeId2comId ns)


nodeId2comId :: [LouvainNode] -> Map LouvainNodeId CommunityId
nodeId2comId ns = fromList [ (nId,cId) | LouvainNode nId cId <- ns]


groupEdges :: Map LouvainNodeId CommunityId
           -> Map (LouvainNodeId, LouvainNodeId) Double
           -> Map (CommunityId, CommunityId) [((LouvainNodeId, LouvainNodeId), Double)]
groupEdges m = fromListWith (<>)
             . catMaybes
             . map (\((n1,n2), d)
                     -> let 
                          n1n2_m = (,) <$> lookup n1 m <*> lookup n2 m
                          n1n2_d = Just [((n1,n2),d)]
                        in (,) <$> n1n2_m <*> n1n2_d
                    )
             . toList

-- | TODO : sortOn Confluence
filterComs :: Bridgeness
           -> Map (CommunityId, CommunityId) [((LouvainNodeId, LouvainNodeId), Double)]
           -> Map (CommunityId, CommunityId) [((LouvainNodeId, LouvainNodeId), Double)]
filterComs b m = mapWithKey filter' m
  where
    filter' (c1,c2) a
      | c1 == c2  = a
      | otherwise = take n $ sortOn (Down . snd) a
          where
            n = round $ 100 * b * a' / t
            a'= fromIntegral $ length a
            t = fromIntegral $ length $ concat $ elems m


