{-| Module      : Gargantext.Core.Viz.Graph.MaxClique
Description : MaxCliques function
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

- First written by Bruno Gaume in Python        (see below for details)
- Then  written by Alexandre Delanoë in Haskell (see below for details)

# By Bruno Gaume:
def fast_maximal_cliques(g):

    def rec_maximal_cliques(g, subv):
        mc = []
        if subv == []: # stop condition
            return [[]]
        else :
            for i in range(len(subv)):
                newsubv = [j for j in subv[i+1:len(subv)]
                                   if (j in g.neighbors(subv[i]))]
                mci = rec_maximal_cliques(g, newsubv)
                for x in mci:
                    x.append(subv[i])
                    mc.append(x)
            return mc

    def purge(clust):
        clustset = [set(x) for x in clust]
        new_clust = []
        for i in range(len(clustset)):
            ok = True
            for j in range(len(clustset)):
                if clustset[i].issubset(clustset[j]) and (not (len(clustset[i]) == len(clustset[j])) ):
                    ok = False
            if ok and (not (clustset[i] in new_clust)):
                new_clust.append(clustset[i])
        return [list(x) for x in new_clust]

    # to optimize : rank the vertices on the degrees
    subv = [(v.index, v.degree()) for v in g.vs()]
    subv.sort(key = lambda z:z[1])
    subv = [x for (x, y) in subv]
    return purge(rec_maximal_cliques(g, subv))

-}



module Gargantext.Core.Viz.Graph.MaxClique
  where

import Data.Maybe (catMaybes)
import Gargantext.Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortOn, nub, concat)
import Data.Set (Set)
import Data.Set (fromList, toList, isSubsetOf)
import Data.Graph.Inductive hiding (Graph, neighbors, subgraph, (&))
import Gargantext.Core.Viz.Graph.FGL (Graph_Undirected, degree, neighbors, mkGraphUfromEdges)
import Gargantext.Core.Viz.Graph.Tools (cooc2graph', Threshold)
import Gargantext.Core.Viz.Graph.Distances (Distance)
import Gargantext.Core.Viz.Graph.Index (createIndices, toIndex)
type Graph = Graph_Undirected
type Neighbor = Node


-- | getMaxCliques
-- TODO chose distance order
getMaxCliques :: Ord a => Distance -> Threshold -> Map (a, a) Int -> [[a]]
getMaxCliques d t m = map fromIndices $ getMaxCliques' t m'
  where
    m'          = toIndex to m
    (to,from)   = createIndices m
    fromIndices = catMaybes . map (\n -> Map.lookup n from)

    getMaxCliques' :: Threshold -> Map (Int, Int) Int -> [[Int]]
    getMaxCliques' t' n = maxCliques graph
      where
        graph = mkGraphUfromEdges (Map.keys n')
        n'    = cooc2graph' d t' n

maxCliques :: Graph -> [[Node]]
maxCliques g = map (\n -> subMaxCliques g (n:ns)) ns & concat & takeMax
  where
    ns :: [Node]
    ns = sortOn (degree g) $ nodes g

    subMaxCliques :: Graph -> [Node] -> [[Node]]
    subMaxCliques _ []  = [[]]
    subMaxCliques g' (x:xs) = add x $ subMaxCliques g' ns'
      where
        ns' = [n | n <- xs, elem n $ neighborsOut g' x]

        add :: Node -> [[Node]] -> [[Node]]
        add n [] = [[n]]
        add n (m:ms) = [n:m] <> add n ms
        -- | Note, it is same as :
        -- add n ns = map (\m -> n : m) ns
        -- -- (but using pattern matching and recursivity)
        -- -- (map is redefined in fact)

        -- | To be sure self is not in neighbors of self
        -- (out to exclude the self)
        neighborsOut :: Graph -> Node -> [Node]
        neighborsOut g'' n = filter (/= n) $ neighbors g'' n


    takeMax :: [[Node]] -> [[Node]]
    takeMax = map toList
            . purge
            . map fromList
            . sortOn length
            . nub 
      where
        purge :: [Set Node] -> [Set Node]
        purge [] = []
        purge (x:xs) = x' <> purge xs
          where
            x' = if all (== False) (map (isSubsetOf x) xs)
                    then [x]
                    else []


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
