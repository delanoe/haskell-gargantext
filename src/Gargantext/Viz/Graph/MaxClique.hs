{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Viz.Graph.MaxClique where

import Gargantext.Prelude
import Data.List (sortOn, nub)
import Data.Bool
import Data.Graph.Inductive hiding (Graph, neighbors, subgraph)
import qualified Data.Graph.Inductive as DGI
import Gargantext.Viz.Graph.FGL (Graph_Undirected, degree, neighbors, mkGraphUfromEdges)
import qualified Data.Set as Set

type Graph = Graph_Undirected
type Neighbor = Node

subgraph g ns = DGI.subgraph ns g

subGraphOn :: Graph -> Node -> Graph
subGraphOn g n = subgraph g (filter (/= n) $ neighbors g n)

maximalClique :: Graph -> [Node] -> [[Node]]
maximalClique _ []  = [[]]
maximalClique _ [n] = [[n]]

cliqueFinder :: Graph -> [[Node]]
cliqueFinder = undefined


{-
------------------------------------------------------------------------
-- TODO: filter subset de cliques 
maxClique :: Graph -> [[Node]]
maxClique g = filterClique g 
            $ map (maxCliqueOn g) (nodes g)

------------------------------------------------------------------------

-- TODO: ask Bruno
-- copier python
filterClique :: Graph -> [Set.Set Node] -> [Set.Set Node]
filterClique = undefined

------------------------------------------------------------------------

type CliqueMax = [Node]

maxCliqueOn :: Graph -> Node -> [CliqueMax]
maxCliqueOn = undefined

maxCliqueOn' :: Graph -> Node -> [Node] -> [CliqueMax]
maxCliqueOn' g n []  = [[n]]
maxCliqueOn' g n [m] = if (neighbors g n = [m]) 
                          then [n,m]
                          else maxCliqueOn' g n [] <> maxCliqueOn' g m []
maxCliqueOn' g n (x:xs) = undefined 


stopClique :: Graph -> Node -> [Node] -> [Node]
-- no self, no reflexivity
stopClique _ n [] = [n]

stopClique g n [m] = if (neighbors g n) == [m]
            then [n,m]
            else []
stopClique g n ns = case all (\n' -> clique g n == clique g n') (x:xs) of
                      True  -> n : ns
                      -- False -> stopClique g x xs
                      False -> stopClique g x xs
      where
        (x:xs) = sort g ns

subGraph :: Graph -> Node -> Graph
subGraph g n = mkGraphUfromEdges (edges voisin <> edges g n)

-}
------------------------------------------------------------------------
-- Some Tools
-- 
{-
sortWith :: (Node -> Node -> Ord) -> Graph -> [Node] -> [Node]
sortWith f g ns = undefined
-}

sort :: Graph -> [Node] -> [Node]
sort _ [] = []
sort g ns = sortOn (degree g) ns

areEdged = areNeighbors
areNeighbors :: Graph -> Node -> Node -> Bool
areNeighbors g n m = neighbors g n == [m]

------------------------------------------------------------------------

test_graph :: Graph
-- test_graph = mkGraphUfromEdges [(1,1), (2,2), (3,3)]
test_graph = mkGraphUfromEdges [(1,2), (3,3)]

test_graph' :: Graph
test_graph' = mkGraphUfromEdges [(1,2), (3,3), (3,2)]

test_graph'' :: Graph
test_graph'' = mkGraphUfromEdges [(1,2), (2,3), (1,3)]

test_graph''' :: Graph
test_graph''' = mkGraphUfromEdges [ (4,1)
                                  , (4,2)
                                  , (3,1)
                                  , (3,2)
                                  , (2,1)
                                  ]
