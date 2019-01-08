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
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO(liftIO))
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

import Data.Swagger

import Gargantext.Prelude
import Gargantext.Core.Types (Label)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Types.Node (NodeId)

import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck (elements)

------------------------------------------------------------------------

data TypeNode = Terms | Unknown
  deriving (Show, Generic)

$(deriveJSON (unPrefix "") ''TypeNode)
instance ToSchema TypeNode

data Attributes = Attributes { clust_default :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''Attributes)
instance ToSchema Attributes

data Node = Node { node_size  :: Int
                 , node_type  :: TypeNode
                 , node_id    :: Text
                 , node_label :: Text
                 , node_attributes :: Attributes
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "node_") ''Node)
instance ToSchema Node where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 5 fieldLabel}


data Edge = Edge { edge_source :: Text
                 , edge_target :: Text
                 , edge_weight :: Double
                 , edge_id     :: Text
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "edge_") ''Edge)
instance ToSchema Edge where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 5 fieldLabel}

---------------------------------------------------------------
data LegendField = LegendField { _lf_id :: Int
                               , _lf_color :: Text
                               , _lf_label :: Text
   } deriving (Show, Generic)
$(deriveJSON (unPrefix "_lf_") ''LegendField)

instance ToSchema LegendField where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}

makeLenses ''LegendField
--
data GraphMetadata = GraphMetadata { _gm_title    :: Text   -- title of the graph
                                   , _gm_corpusId :: [NodeId]  -- we can map with different corpus
                                   , _gm_legend :: [LegendField] -- legend of the Graph
                                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_gm_") ''GraphMetadata)
instance ToSchema GraphMetadata where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}
makeLenses ''GraphMetadata


data Graph = Graph { _graph_nodes :: [Node]
                   , _graph_edges :: [Edge]
                   , _graph_metadata :: Maybe GraphMetadata
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_graph_") ''Graph)
makeLenses ''Graph

instance ToSchema Graph where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 7 fieldLabel}


defaultGraph :: Graph
defaultGraph = Graph {_graph_nodes = [Node {node_size = 4, node_type = Terms, node_id = pack "0", node_label = pack "animal", node_attributes = Attributes {clust_default = 0}},Node {node_size = 3, node_type = Terms, node_id = pack "1", node_label = pack "bird", node_attributes = Attributes {clust_default = 0}},Node {node_size = 2, node_type = Terms, node_id = pack "2", node_label = pack "boy", node_attributes = Attributes {clust_default = 1}},Node {node_size = 2, node_type = Terms, node_id = pack "3", node_label = pack "dog", node_attributes = Attributes {clust_default = 0}},Node {node_size = 2, node_type = Terms, node_id = pack "4", node_label = pack "girl", node_attributes = Attributes {clust_default = 1}},Node {node_size = 4, node_type = Terms, node_id = pack "5", node_label = pack "human body", node_attributes = Attributes {clust_default = 1}},Node {node_size = 3, node_type = Terms, node_id = pack "6", node_label = pack "object", node_attributes = Attributes {clust_default = 2}},Node {node_size = 2, node_type = Terms, node_id = pack "7", node_label = pack "pen", node_attributes = Attributes {clust_default = 2}},Node {node_size = 2, node_type = Terms, node_id = pack "8", node_label = pack "table", node_attributes = Attributes {clust_default = 2}}], _graph_edges = [Edge {edge_source = pack "0", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "0"},Edge {edge_source = pack "1", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "1"},Edge {edge_source = pack "1", edge_target = pack "1", edge_weight = 1.0, edge_id = pack "2"},Edge {edge_source = pack "2", edge_target = pack "2", edge_weight = 1.0, edge_id = pack "3"},Edge {edge_source = pack "2", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "4"},Edge {edge_source = pack "3", edge_target = pack "0", edge_weight = 1.0, edge_id = pack "5"},Edge {edge_source = pack "3", edge_target = pack "1", edge_weight = 1.0, edge_id = pack "6"},Edge {edge_source = pack "3", edge_target = pack "3", edge_weight = 1.0, edge_id = pack "7"},Edge {edge_source = pack "4", edge_target = pack "4", edge_weight = 1.0, edge_id = pack "8"},Edge {edge_source = pack "4", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "9"},Edge {edge_source = pack "5", edge_target = pack "5", edge_weight = 1.0, edge_id = pack "10"},Edge {edge_source = pack "6", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "11"},Edge {edge_source = pack "7", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "12"},Edge {edge_source = pack "7", edge_target = pack "7", edge_weight = 1.0, edge_id = pack "13"},Edge {edge_source = pack "8", edge_target = pack "6", edge_weight = 1.0, edge_id = pack "14"},Edge {edge_source = pack "8", edge_target = pack "7", edge_weight = 1.0, edge_id = pack "15"},Edge {edge_source = pack "8", edge_target = pack "8", edge_weight = 1.0, edge_id = pack "16"}], _graph_metadata = Nothing}

-- | Intances for the mack
instance Arbitrary Graph where
  arbitrary = elements $ [defaultGraph]


-----------------------------------------------------------
-- V3 Gargantext Version

data AttributesV3 = AttributesV3 { cl :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''AttributesV3)

data NodeV3 = NodeV3 { no_id :: Int
                     , no_at :: AttributesV3
                     , no_s  :: Int
                     , no_lb :: Text
                     }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "no_") ''NodeV3)

data EdgeV3 = EdgeV3 { eo_s :: Int
                     , eo_t :: Int
                     , eo_w :: Text
                     }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "eo_") ''EdgeV3)

data GraphV3 = GraphV3 { go_links :: [EdgeV3]
                       , go_nodes :: [NodeV3]
                       }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "go_") ''GraphV3)

----------------------------------------------------------
-- | From data to Graph
-- FIXME: distance should not be a map since we just "toList" it (same as cLouvain)
data2graph :: [(Label, Int)] -> Map (Int, Int) Int
                             -> Map (Int, Int) Double
                             -> [LouvainNode]
              -> Graph
data2graph labels coocs distance partitions = Graph nodes edges Nothing
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

graphV3ToGraph :: GraphV3 -> Graph
graphV3ToGraph (GraphV3 links nodes) = Graph (map nodeV32node nodes) (zipWith linkV32edge [1..] links) Nothing
  where
    nodeV32node :: NodeV3 -> Node
    nodeV32node (NodeV3 no_id' (AttributesV3 cl') no_s' no_lb')
                = Node no_s' Terms (cs $ show no_id') no_lb' (Attributes cl')
    
    linkV32edge :: Int -> EdgeV3 -> Edge
    linkV32edge n (EdgeV3 eo_s' eo_t' eo_w') = Edge (cs $ show eo_s') (cs $ show eo_t') ((T.read $ T.unpack eo_w') :: Double) (cs $ show n)


graphV3ToGraphWithFiles :: FilePath -> FilePath -> IO ()
graphV3ToGraphWithFiles g1 g2 = do
  -- GraphV3 <- IO Fichier
  graph <- DBL.readFile g1
  let newGraph = case DA.decode graph :: Maybe GraphV3 of
        Nothing -> panic (T.pack "no graph")
        Just new -> new

  DBL.writeFile g2 (DA.encode $ graphV3ToGraph newGraph)

readGraphFromJson :: MonadIO m => FilePath -> m (Maybe Graph)
readGraphFromJson fp = do
  graph <- liftIO $ DBL.readFile fp
  pure $ DA.decode graph
