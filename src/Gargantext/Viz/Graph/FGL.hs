{-| Module      : Gargantext.Viz.Graph.FGL
Description : FGL main functions used in Garg
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main FGL funs/types to ease portability with IGraph.

-}

{-# LANGUAGE ConstraintKinds   #-}

module Gargantext.Viz.Graph.FGL where

import Gargantext.Prelude
import qualified Data.Graph.Inductive as FGL
import Data.List as List
------------------------------------------------------------------
-- | Main Types

type Graph_Undirected = FGL.Gr () ()
type Graph_Directed   = FGL.Gr () ()

type Graph = FGL.Graph
type Node = FGL.Node -- Int
type Edge = FGL.Edge -- (Int, Int)

------------------------------------------------------------------
-- | Main Functions
mkGraph :: [Node] -> [Edge] -> Graph_Undirected
mkGraph = FGL.mkUGraph

neighbors :: Graph gr => gr a b -> Node -> [Node]
neighbors = FGL.neighbors

-- | TODO bug: if graph is undirected, we need to filter
-- nub . (map (\(n1,n2) -> if n1 < n2 then (n1,n2) else (n2,n1))) . FGL.edges
edges :: Graph gr => gr a b -> [Edge]
edges = FGL.edges

nodes :: Graph gr => gr a b -> [Node]
nodes = FGL.nodes

------------------------------------------------------------------------
-- | Graph Tools

filterNeighbors :: Graph_Undirected -> Node -> [Node]
filterNeighbors g n = List.nub $ neighbors g n

-- Q: why not D.G.I.deg ? (Int as result)
degree :: Graph_Undirected -> Node -> Double
degree g n = fromIntegral $ List.length (filterNeighbors g n)

vcount :: Graph_Undirected -> Double
vcount = fromIntegral . List.length . List.nub . nodes

-- | TODO tests, optim and use IGraph library, fix IO ?
ecount :: Graph_Undirected -> Double
ecount = fromIntegral . List.length . List.nub . edges


------------------------------------------------------------------
-- | Main sugared functions
mkGraphUfromEdges :: [(Int, Int)] -> Graph_Undirected
mkGraphUfromEdges es = mkGraph ns es
  where
    ns = List.nub (a <> b)
      where
        (a, b) = List.unzip es

