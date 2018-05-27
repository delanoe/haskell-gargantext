{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Data.GexfParser (importGraphFromGexf)
 where

import Text.XML.HXT.Core
-- import qualified Data.Graph as DataGraph
import qualified Data.Graph.Inductive as FGL
import System.Environment (getArgs)

data Graph = Graph
  { graphId :: String,
    nodes :: [String],
    edges :: [(String, String)] -- (Source, target)
  }
  deriving (Show, Eq)

atTag tag = deep (isElem >>> hasName tag)

parseEdges = atTag "edge" >>>
  proc e -> do
      source <- getAttrValue "source" -< e
      target <- getAttrValue "target" -< e
      returnA -< (source, target)

parseNodes = atTag "node" >>>
  proc n -> do
      nodeId <- getAttrValue "id" -< n
      returnA -< nodeId

parseGraph = atTag "graph" >>>
  proc g -> do
      graphId <- getAttrValue "id" -< g
      nodes <- listA parseNodes -< g
      edges <- listA parseEdges -< g
      returnA -< Graph{graphId=graphId, nodes=nodes, edges=edges}

getEdges = atTag "edge" >>> getAttrValue "source"

-- Get targets for a single node in a Graph
getTargets :: String -> Graph -> [String]
getTargets source graph = map snd $ filter ((==source).fst) $ edges graph

-- Convert a graph node into a Data.Graph-usable
-- getDataGraphNode :: Graph -> String -> (String, String, [String])
-- getDataGraphNode graph node = (node, node, getTargets node graph)
-- 
-- 
-- getDataGraphNode' :: Graph -> String -> (Int, [Int])
-- getDataGraphNode' graph node = (read node, Prelude.map read (getTargets node graph))
-- 
-- -- Convert a Graph instance into a Data.Graph list of (node, nodeid, edge) tuples
-- getDataGraphNodeList :: Graph -> [(String, String, [String])]
-- getDataGraphNodeList graph = map (getDataGraphNode graph) (nodes graph)
-- 
-- getDataGraphNodeList' :: Graph -> [(Int, [Int])]
-- getDataGraphNodeList' graph = map (getDataGraphNode' graph) (nodes graph)
-- 
-- --  Convert Graph structure to Data.Graph-importable tuple list
-- importGraph :: FilePath -> IO [(Int, [Int])]
-- importGraph file = do
--     graphs <- runX (readDocument [withValidate no] file >>> parseGraph)
--     let graphEdges = getDataGraphNodeList' $ head graphs
--     return graphEdges
-- 
--importGraph' :: FilePath -> IO [(Int, [Int])]
importGraph' file = runX (readDocument [withValidate no] file >>> parseGraph)

importGraphFromGexf :: FilePath -> IO [FGL.LEdge Double]
importGraphFromGexf file = Prelude.map (\(a,b) -> (read a, read b, 1)) <$> edges <$> head <$> importGraph' file


--main :: IO()
-- main = do
--     [file] <- getArgs
--     importGraph file >>= print
-- 
    -- Convert to a Data.Graph
    -- let (graph, vertexMap) = DataGraph.graphFromEdges' graphEdges
    -- Example of what to do with the Graph: Print vertices
    -- print $ map ((\ (vid, _, _) -> vid) . vertexMap) (DataGraph.vertices graph)
