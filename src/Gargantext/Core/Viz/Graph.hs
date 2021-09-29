{-|
Module      : Gargantext.Core.Viz.Graph
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Core.Viz.Graph
  where

import Data.ByteString.Lazy as DBL (readFile, writeFile)
import Data.Text (pack)
import GHC.IO (FilePath)

import qualified Data.Aeson as DA
import qualified Data.Text as T
import qualified Text.Read as T

import Gargantext.Core.Types (ListId)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Methods.Distances (GraphMetric)
import Gargantext.Prelude


data TypeNode = Terms | Unknown
  deriving (Show, Generic)

instance ToJSON TypeNode
instance FromJSON TypeNode
instance ToSchema TypeNode

data Attributes = Attributes { clust_default :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''Attributes)
instance ToSchema Attributes

data Node = Node { node_size  :: Int
                 , node_type  :: TypeNode -- TODO NgramsType | Person
                 , node_id    :: Text     -- TODO NgramId
                 , node_label :: Text
                 , node_x_coord :: Double
                 , node_y_coord :: Double
                 , node_attributes :: Attributes
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "node_") ''Node)
instance ToSchema Node where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "node_")


data Edge = Edge { edge_source :: Text
                 , edge_target :: Text
                 , edge_weight :: Double
                 , edge_confluence :: Double
                 , edge_id     :: Text
                 }
  deriving (Show, Generic)

$(deriveJSON (unPrefix "edge_") ''Edge)

instance ToSchema Edge where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "edge_")

---------------------------------------------------------------
data LegendField = LegendField { _lf_id :: Int
                               , _lf_color :: Text
                               , _lf_label :: Text
   } deriving (Show, Generic)
$(deriveJSON (unPrefix "_lf_") ''LegendField)

instance ToSchema LegendField where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_lf_")

makeLenses ''LegendField
---------------------------------------------------------------
type Version = Int
data ListForGraph =
  ListForGraph { _lfg_listId  :: ListId
               , _lfg_version :: Version
               } deriving (Show, Generic)
$(deriveJSON (unPrefix "_lfg_") ''ListForGraph)

instance ToSchema ListForGraph where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_lfg_")

makeLenses ''ListForGraph

--
data GraphMetadata =
  GraphMetadata { _gm_title            :: Text          -- title of the graph
                , _gm_metric           :: GraphMetric
                , _gm_corpusId         :: [NodeId]      -- we can map with different corpus
                , _gm_legend           :: [LegendField] -- legend of the Graph
                , _gm_list             :: ListForGraph
                , _gm_startForceAtlas  :: Bool
                -- , _gm_version       :: Int
                }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_gm_") ''GraphMetadata)
instance ToSchema GraphMetadata where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gm_")
makeLenses ''GraphMetadata


data Graph = Graph { _graph_nodes    :: [Node]
                   , _graph_edges    :: [Edge]
                   , _graph_metadata :: Maybe GraphMetadata
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_graph_") ''Graph)
makeLenses ''Graph

instance ToSchema Graph where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_graph_")

-- | Intances for the mock
instance Arbitrary Graph where
  arbitrary = elements $ [defaultGraph]

defaultGraph :: Graph
defaultGraph = Graph {_graph_nodes = [Node {node_x_coord=0, node_y_coord=0, node_size = 4, node_type = Terms, node_id = pack "0", node_label = pack "animal", node_attributes = Attributes {clust_default = 0}},Node {node_x_coord=0, node_y_coord=0, node_size = 3, node_type = Terms, node_id = pack "1", node_label = pack "bird", node_attributes = Attributes {clust_default = 0}},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = Terms, node_id = pack "2", node_label = pack "boy", node_attributes = Attributes {clust_default = 1}},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = Terms, node_id = pack "3", node_label = pack "dog", node_attributes = Attributes {clust_default = 0}},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = Terms, node_id = pack "4", node_label = pack "girl", node_attributes = Attributes {clust_default = 1}},Node {node_x_coord=0, node_y_coord=0, node_size = 4, node_type = Terms, node_id = pack "5", node_label = pack "human body", node_attributes = Attributes {clust_default = 1}},Node {node_x_coord=0, node_y_coord=0, node_size = 3, node_type = Terms, node_id = pack "6", node_label = pack "object", node_attributes = Attributes {clust_default = 2}},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = Terms, node_id = pack "7", node_label = pack "pen", node_attributes = Attributes {clust_default = 2}},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = Terms, node_id = pack "8", node_label = pack "table", node_attributes = Attributes {clust_default = 2}}], _graph_edges = [Edge {edge_source = pack "0", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "0"},Edge {edge_source = pack "1", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "1"},Edge {edge_source = pack "1", edge_target = pack "1", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "2"},Edge {edge_source = pack "2", edge_target = pack "2", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "3"},Edge {edge_source = pack "2", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "4"},Edge {edge_source = pack "3", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "5"},Edge {edge_source = pack "3", edge_target = pack "1", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "6"},Edge {edge_source = pack "3", edge_target = pack "3", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "7"},Edge {edge_source = pack "4", edge_target = pack "4", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "8"},Edge {edge_source = pack "4", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "9"},Edge {edge_source = pack "5", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "10"},Edge {edge_source = pack "6", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "11"},Edge {edge_source = pack "7", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "12"},Edge {edge_source = pack "7", edge_target = pack "7", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "13"},Edge {edge_source = pack "8", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "14"},Edge {edge_source = pack "8", edge_target = pack "7", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "15"},Edge {edge_source = pack "8", edge_target = pack "8", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "16"}], _graph_metadata = Nothing}


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

-----------------------------------------------------------
data Camera = Camera { _camera_ratio :: Double
                     , _camera_x     :: Double
                     , _camera_y     :: Double }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_camera_") ''Camera)
makeLenses ''Camera

instance ToSchema Camera where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_camera_")

-----------------------------------------------------------
data HyperdataGraph =
  HyperdataGraph { _hyperdataGraph :: !(Maybe Graph)
                 , _hyperdataCamera :: !(Maybe Camera)
                 } deriving (Show, Generic)
$(deriveJSON (unPrefix "_") ''HyperdataGraph)
instance ToSchema HyperdataGraph where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")

defaultHyperdataGraph :: HyperdataGraph 
defaultHyperdataGraph = HyperdataGraph Nothing Nothing


instance Hyperdata HyperdataGraph
makeLenses ''HyperdataGraph

instance FromField HyperdataGraph
  where
    fromField = fromField'

instance DefaultFromField PGJsonb HyperdataGraph
  where
    defaultFromField = fieldQueryRunnerColumn

-----------------------------------------------------------
-- This type is used to return graph via API
-- hyperdataGraphAPI field is not a Maybe anymore – graph is always computed
data HyperdataGraphAPI =
  HyperdataGraphAPI { _hyperdataAPIGraph  :: Graph
                    , _hyperdataAPICamera :: !(Maybe Camera)
                    } deriving (Show, Generic)
$(deriveJSON (unPrefix "_hyperdataAPI") ''HyperdataGraphAPI)
instance ToSchema HyperdataGraphAPI where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hyperdataAPI")

makeLenses ''HyperdataGraphAPI

instance FromField HyperdataGraphAPI
  where
    fromField = fromField'

-----------------------------------------------------------
graphV3ToGraph :: GraphV3 -> Graph
graphV3ToGraph (GraphV3 links nodes) = Graph (map nodeV32node nodes) (zipWith linkV32edge [1..] links) Nothing
  where
    nodeV32node :: NodeV3 -> Node
    nodeV32node (NodeV3 no_id' (AttributesV3 cl') no_s' no_lb')
                = Node no_s' Terms (cs $ show no_id') no_lb' 0 0 (Attributes cl')

    linkV32edge :: Int -> EdgeV3 -> Edge
    linkV32edge n (EdgeV3 eo_s' eo_t' eo_w') = Edge (cs $ show eo_s')
                                                    (cs $ show eo_t')
                                                    ((T.read $ T.unpack eo_w') :: Double)
                                                    0.5
                                                    (cs $ show n)


graphV3ToGraphWithFiles :: FilePath -> FilePath -> IO ()
graphV3ToGraphWithFiles g1 g2 = do
  -- GraphV3 <- IO Fichier
  graph <- DBL.readFile g1
  let newGraph = case DA.decode graph :: Maybe GraphV3 of
        Nothing -> panic (T.pack "no graph")
        Just new -> new

  DBL.writeFile g2 (DA.encode $ graphV3ToGraph newGraph)

readGraphFromJson :: MonadBase IO m => FilePath -> m (Maybe Graph)
readGraphFromJson fp = do
  graph <- liftBase $ DBL.readFile fp
  pure $ DA.decode graph
