{-|
Module      : Gargantext.Viz.Graph.Louvain
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Viz.Graph.Louvain
  where

import Gargantext.Prelude
import Data.Map (Map, fromList)
import Data.Graph.Clustering.Louvain.Utils (LouvainNode(..))


type LouvainNodeId = Int
type CommunityId   = Int

nodeId2comId :: [LouvainNode] -> Map LouvainNodeId CommunityId
nodeId2comId ns = fromList [(nId,cId) | LouvainNode nId cId <- ns]

comId2nodeId :: [LouvainNode] -> Map CommunityId LouvainNodeId
comId2nodeId ns = fromList [(cId,nId) | LouvainNode nId cId <- ns]


