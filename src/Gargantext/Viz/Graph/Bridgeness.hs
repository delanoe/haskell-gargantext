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

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Viz.Graph.Bridgeness (bridgeness)
  where
--import GHC.Base (Semigroup)
import Gargantext.Prelude
--import Data.Tuple.Extra (swap)
--import Gargantext.Viz.Graph
import Data.Map (Map, fromListWith, lookup, fromList, delete, toList, mapKeys, mapWithKey, elems)
import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Data.List (concat, sortOn)
import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))


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
groupEdges m = mapKeys fromJust
             . delete Nothing
             . fromListWith (<>)
             . map (\((n1,n2), d)
                     -> ((,) <$> lookup n1 m
                             <*> lookup n2 m
                        , [((n1,n2),d)]
                        )
                   )
             . toList

filterComs :: Bridgeness
           -> Map (CommunityId, CommunityId) [((LouvainNodeId, LouvainNodeId), Double)]
           -> Map (CommunityId, CommunityId) [((LouvainNodeId, LouvainNodeId), Double)]
filterComs b m = mapWithKey filter' m
  where
    filter' (c1,c2) a = case c1 == c2 of
      True  -> a
      False -> take n $ sortOn snd a
        where
          n = round $ b * a' / t
          a'= fromIntegral $ length a
          t = fromIntegral $ length $ concat $ elems m


