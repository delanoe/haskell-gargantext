{-| Module      : Gargantext.Viz.Graph.IGraph
Description : IGraph main functions used in Garg
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main IGraph funs/types to ease portability with FGL.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}

module Gargantext.Viz.Graph.IGraph where

import Data.Serialize (Serialize)
import Data.Singletons (SingI)
import Gargantext.Prelude
import IGraph hiding (mkGraph, neighbors, edges, nodes, Node, Graph)
import qualified IGraph as IG
import qualified Data.List as List

------------------------------------------------------------------
-- | Main Types
type Graph_Undirected = IG.Graph 'U () ()
type Graph_Directed   = IG.Graph 'D () ()

type Node = IG.Node
type Graph = IG.Graph

------------------------------------------------------------------
-- | Main Functions

mkGraph :: (SingI d, Ord v,
             Serialize v, Serialize e) =>
     [v] -> [LEdge e] -> IG.Graph d v e
mkGraph = IG.mkGraph

neighbors :: IG.Graph d v e -> IG.Node -> [Node]
neighbors = IG.neighbors

edges :: IG.Graph d v e -> [Edge]
edges = IG.edges

nodes :: IG.Graph d v e -> [Node]
nodes = IG.nodes

------------------------------------------------------------------
-- | Main sugared functions

mkGraphUfromEdges :: [(Int, Int)] -> Graph_Undirected
mkGraphUfromEdges es = mkGraph (List.replicate n ()) $ zip es $ repeat ()
  where
    (a,b) = List.unzip es
    n = List.length (List.nub $ a <> b)

mkGraphDfromEdges :: [(Int, Int)] -> Graph_Directed
mkGraphDfromEdges = undefined


