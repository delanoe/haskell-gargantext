{-|
Module      : Gargantext.Viz.Graph.Tools
Description : Tools to build Graph
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Viz.Graph.Tools
  where

import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))
import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Viz.Graph (Graph(..))
import Gargantext.Viz.Graph -- (Graph(..))
import Gargantext.Viz.Graph.Bridgeness (bridgeness)
import Gargantext.Viz.Graph.Distances.Matrice (measureConditional)
import Gargantext.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map)
import qualified Data.Map as Map

cooc2graph :: (Map (Text, Text) Int) -> IO Graph
cooc2graph myCooc = do
  let (ti, _) = createIndices myCooc
      myCooc4 = toIndex ti myCooc
      matCooc = map2mat (0) (Map.size ti) myCooc4
      distanceMat = measureConditional matCooc
      distanceMap = Map.map (\_ -> 1) $ Map.filter (>0) $ mat2map distanceMat

  partitions <- case Map.size distanceMap > 0 of
    True  -> cLouvain distanceMap
    False -> panic "Text.Flow: DistanceMap is empty"

  let distanceMap' = bridgeness 300 partitions distanceMap

  pure $ data2graph (Map.toList ti) myCooc4 distanceMap' partitions


----------------------------------------------------------
-- | From data to Graph
-- FIXME: distance should not be a map since we just "toList" it (same as cLouvain)
data2graph :: [(Text, Int)] -> Map (Int, Int) Int
                            -> Map (Int, Int) Double
                            -> [LouvainNode]
              -> Graph
data2graph labels coocs distance partitions = Graph nodes edges Nothing
  where
    community_id_by_node_id = Map.fromList [ (n, c) | LouvainNode n c <- partitions ]
    nodes = [ Node { node_size = maybe 0 identity (Map.lookup (n,n) coocs)
                   , node_type = Terms -- or Unknown
                   , node_id    = cs (show n)
                   , node_label = l
                   , node_attributes =
                     Attributes { clust_default = maybe 0 identity 
                                (Map.lookup n community_id_by_node_id) } }
            | (l, n) <- labels ]
    edges = [ Edge { edge_source = cs (show s)
                   , edge_target = cs (show t)
                   , edge_weight = w
                   , edge_id     = cs (show i) }
            | (i, ((s,t), w)) <- zip ([0..]::[Integer]) (Map.toList distance) ]


