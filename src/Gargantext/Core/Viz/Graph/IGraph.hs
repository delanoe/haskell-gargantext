{-| Module      : Gargantext.Core.Viz.Graph.IGraph
Description : IGraph main functions used in Garg
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main IGraph funs/types to ease portability with FGL.

Reference:
* Gábor Csárdi, Tamás Nepusz: The igraph software package for complex network research. InterJournal Complex Systems, 1695, 2006.

-}


module Gargantext.Core.Viz.Graph.IGraph where

import Data.Serialize (Serialize)
import Data.Singletons (SingI)
import Gargantext.Prelude
import IGraph hiding (mkGraph, neighbors, edges, nodes, Node, Graph)
import IGraph.Algorithms.Clique as IAC
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

-- | Tools
maximalCliques :: IG.Graph d v e -> [[Int]]
maximalCliques g = IAC.maximalCliques g (min',max')
  where
    min' = 0
    max' = 0

------------------------------------------------------------------
-- | Main sugared functions
mkGraphUfromEdges :: [(Int, Int)] -> Graph_Undirected
mkGraphUfromEdges es = mkGraph (List.replicate n ()) $ zip es $ repeat ()
  where
    (a,b) = List.unzip es
    n = List.length (List.nub $ a <> b)

mkGraphDfromEdges :: [(Int, Int)] -> Graph_Directed
mkGraphDfromEdges = undefined


