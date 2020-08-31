{-|
Module      : Gargantext.Core.Viz.Graph.Legend
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Viz.Graph.Legend
  where

{-
import Data.Ord (Down(..))
import Gargantext.Prelude
import Data.Map (Map, fromListWith, lookup, toList, mapWithKey, elems)
import qualified Data.Map as DM
import Data.Maybe (catMaybes)
import Data.List (concat, sortOn)
import Gargantext.Core.Viz.Graph.Louvain (LouvainNodeId, CommunityId, comId2nodeId)



[LouvainNode] -> Map CommunityId LouvainNodeId
[(CommunityId, [LouvainNodeId])]
sort by length LouvainNodeIds


Cooc -> DGI.Graph
sort [LouvainNodeId] 

subgraph with [LouvainNodeId]
-> prendre le noeud le mieux connecté (degree to start with)

Map NodeId Label
-> map [LouvainNodeId] -> [(CommunityId, take 3 [Label])]

use specGen incExc score to order the labels

take 7 [(CommunityId, take 3 [Label])]



-}




