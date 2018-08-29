{-|
Module      : Gargantext.Viz.Graph
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gargantext.Viz.Graph
  where

------------------------------------------------------------------------
import GHC.IO (FilePath)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson as DA

import Data.ByteString.Lazy as DBL (readFile, writeFile)

import Data.Text (Text, pack)
import qualified Text.Read as T
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Swagger (ToSchema)

import Gargantext.Prelude
import Gargantext.Core.Types (Label)
import Gargantext.Core.Utils.Prefix (unPrefix)

import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck (elements)

------------------------------------------------------------------------

data TypeNode = Terms | Unknown
  deriving (Show, Generic)

$(deriveJSON (unPrefix "") ''TypeNode)

data Attributes = Attributes { clust_default :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''Attributes)

data Node = Node { node_size  :: Int
                 , node_type  :: TypeNode
                 , node_id    :: Text
                 , node_label :: Text
                 , node_attributes :: Attributes
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "node_") ''Node)

data Edge = Edge { edge_source :: Text
                 , edge_target :: Text
                 , edge_weight :: Double
                 , edge_id     :: Text
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "edge_") ''Edge)

data Graph = Graph { graph_nodes :: [Node]
                   , graph_edges :: [Edge]
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "graph_") ''Graph)

-- | Intances for Swagger documentation
instance ToSchema Node
instance ToSchema TypeNode
instance ToSchema Attributes
instance ToSchema Edge
instance ToSchema Graph

-- | Intances for the mack
instance Arbitrary Graph where
  arbitrary = elements $ [Graph {graph_nodes = [Node {node_size = 4, node_type = Terms, node_id = pack "0", node_label = pack "animal", node_attributes = Attributes {clust_default = 0}},Node {node_size = 3, node_type = Terms, node_id = pack "1", node_label = pack "bird", node_attributes = Attributes {clust_default = 0}},Node {node_size = 2, node_type = Terms, node_id = pack "2", node_label = pack "boy", node_attributes = Attributes {clust_default = 1}},Node {node_size = 2, node_type = Terms, node_id = pack "3", node_label = pack "dog", node_attributes = Attributes {clust_default = 0}},Node {node_size = 2, node_type = Terms, node_id = pack "4", node_label = pack "girl", node_attributes = Attributes {clust_default = 1}},Node {node_size = 4, node_type = Terms, node_id = pack "5", node_label = pack "human body", node_attributes = Attributes {clust_default = 1}},Node {node_size = 3, node_type = Terms, node_id = pack "6", node_label = pack "object", node_attributes = Attributes {clust_default = 2}},Node {node_size = 2, node_type = Terms, node_id = pack "7", node_label = pack "pen", node_attributes = Attributes {clust_default = 2}},Node {node_size = 2, node_type = Terms, node_id = pack "8", node_label = pack "table", node_attributes = Attributes {clust_default = 2}}], graph_edges = [Edge {edge_source = pack "0", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "0"},Edge {edge_source = pack "1", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "1"},Edge {edge_source = pack "1", edge_target = pack "1", edge_weight = 1.0, edge_id = pack "2"},Edge {edge_source = pack "2", edge_target = pack "2", edge_weight = 1.0, edge_id = pack "3"},Edge {edge_source = pack "2", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "4"},Edge {edge_source = pack "3", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "5"},Edge {edge_source = pack "3", edge_target = pack "1", edge_weight = 1.0, edge_id = pack "6"},Edge {edge_source = pack "3", edge_target = pack "3", edge_weight = 1.0, edge_id = pack "7"},Edge {edge_source = pack "4", edge_target = pack "4", edge_weight = 1.0, edge_id = pack "8"},Edge {edge_source = pack "4", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "9"},Edge {edge_source = pack "5", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "10"},Edge {edge_source = pack "6", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "11"},Edge {edge_source = pack "7", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "12"},Edge {edge_source = pack "7", edge_target = pack "7", edge_weight = 1.0, edge_id = pack "13"},Edge {edge_source = pack "8", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "14"},Edge {edge_source = pack "8", edge_target = pack "7", edge_weight = 1.0, edge_id = pack "15"},Edge {edge_source = pack "8", edge_target = pack "8", edge_weight = 1.0, edge_id = pack "16"}]}]


-----------------------------------------------------------
-- Old Gargantext Version

data AttributesOld = AttributesOld { cl :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''AttributesOld)

data NodeOld = NodeOld { no_id :: Int
                   , no_at :: AttributesOld
                   , no_s :: Int
                   , no_lb :: Text
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "no_") ''NodeOld)

data EdgeOld = EdgeOld { eo_s :: Int
                 , eo_t :: Int
                 , eo_w :: Text
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "eo_") ''EdgeOld)

data GraphOld = GraphOld { 
                           go_links :: [EdgeOld]
                         , go_nodes :: [NodeOld]
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "go_") ''GraphOld)

----------------------------------------------------------
-- | From data to Graph
-- FIXME: distance should not be a map since we just "toList" it (same as cLouvain)
data2graph :: [(Label, Int)] -> Map (Int, Int) Int
                             -> Map (Int, Int) Double
                             -> [LouvainNode]
              -> Graph
data2graph labels coocs distance partitions = Graph nodes edges
  where
    community_id_by_node_id = M.fromList [ (n, c) | LouvainNode n c <- partitions ]
    nodes = [ Node { node_size = maybe 0 identity (M.lookup (n,n) coocs)
                   , node_type = Terms -- or Unknown
                   , node_id = cs (show n)
                   , node_label = T.unwords l
                   , node_attributes = 
                     Attributes { clust_default = maybe 0 identity 
                                (M.lookup n community_id_by_node_id) } }
            | (l, n) <- labels ]
    edges = [ Edge { edge_source = cs (show s)
                   , edge_target = cs (show t)
                   , edge_weight = w
                   , edge_id     = cs (show i) }
            | (i, ((s,t), w)) <- zip ([0..]::[Integer]) (M.toList distance) ]
-----------------------------------------------------------
-----------------------------------------------------------

graphOld2graph :: GraphOld -> Graph
graphOld2graph (GraphOld links nodes) = Graph (map nodeOld2node nodes) (zipWith linkOld2edge [1..] links)
  where
    nodeOld2node :: NodeOld -> Node
    nodeOld2node (NodeOld no_id' (AttributesOld cl') no_s' no_lb')
                = Node no_s' Terms (cs $ show no_id') no_lb' (Attributes cl')
    
    linkOld2edge :: Int -> EdgeOld -> Edge
    linkOld2edge n (EdgeOld eo_s' eo_t' eo_w') = Edge (cs $ show eo_s') (cs $ show eo_t') ((T.read $ T.unpack eo_w') :: Double) (cs $ show n)


graphOld2graphWithFiles :: FilePath -> FilePath -> IO ()
graphOld2graphWithFiles g1 g2 = do
  -- GraphOld <- IO Fichier
  graph <- DBL.readFile g1
  let newGraph = case DA.decode graph :: Maybe GraphOld of
        Nothing -> panic (T.pack "no graph")
        Just new -> new

  DBL.writeFile g2 (DA.encode $ graphOld2graph newGraph)


