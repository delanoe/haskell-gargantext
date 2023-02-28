{-|
Module      : Gargantext.Core.Viz.Graph.Types
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Core.Viz.Graph.Types
  where

import Control.Lens (makeLenses)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Data.Text (Text, pack)
import Gargantext.API.Ngrams.Types (NgramsTerm)
import Gargantext.Core.Methods.Similarities (GraphMetric)
import Gargantext.Core.Types (ListId)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude (Hyperdata)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Prelude (fromField')
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import GHC.Generics (Generic)
import Opaleye (DefaultFromField(..), SqlJsonb, fromPGSFromField)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


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
                 , node_type  :: NgramsType -- TypeNode -- TODO NgramsType | Person
                 , node_id    :: Text     -- TODO NgramId
                 , node_label :: Text
                 , node_x_coord :: Double
                 , node_y_coord :: Double
                 , node_attributes :: Attributes
                 , node_children :: [Text]
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "node_") ''Node)
instance ToSchema Node where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "node_")


data Edge = Edge { edge_confluence :: Double
                 , edge_hidden     :: Maybe Bool
                 , edge_id         :: Text
                 , edge_source     :: Text
                 , edge_target     :: Text
                 , edge_weight     :: Double
                 }
  deriving (Show, Generic)

$(deriveJSON (unPrefix "edge_") ''Edge)

instance ToSchema Edge where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "edge_")

---------------------------------------------------------------
data LegendField = LegendField { _lf_id :: Int
                               , _lf_color :: Text
                               , _lf_label :: Text
                               }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_lf_") ''LegendField)

instance ToSchema LegendField where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_lf_")

makeLenses ''LegendField

---------------------------------------------------------------
data Partite = Partite { _partite_nodes :: HashSet NgramsTerm
                       , _partite_type :: NgramsType
                       }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_partite_") ''Partite)
instance ToSchema Partite where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_partite_")
makeLenses ''Partite


data MultiPartite = MultiPartite { _multipartite_data1 :: Partite
                                 , _multipartite_data2 :: Partite
                                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_multipartite_") ''MultiPartite)
instance ToSchema MultiPartite where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_multipartite_")
makeLenses ''MultiPartite

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

data Strength = Strong | Weak
    deriving (Generic, Eq, Ord, Enum, Bounded, Show)

$(deriveJSON (unPrefix "") ''Strength)
instance ToSchema Strength where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")



data GraphMetadata =
  GraphMetadata { _gm_title            :: Text          -- title of the graph
                , _gm_metric           :: GraphMetric
                , _gm_edgesStrength    :: Maybe Strength
                , _gm_corpusId         :: [NodeId]      -- we can map with different corpus
                , _gm_legend           :: [LegendField] -- legend of the Graph
                , _gm_list             :: ListForGraph
                , _gm_startForceAtlas  :: Bool
                -- , _gm_nodesTypes       :: Maybe (NgramsType, NgramsType)
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
data Camera = Camera { _camera_angle :: Double
                     , _camera_ratio :: Double
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



instance Hyperdata HyperdataGraph
makeLenses ''HyperdataGraph

instance FromField HyperdataGraph
  where
    fromField = fromField'

instance DefaultFromField SqlJsonb HyperdataGraph
  where
    defaultFromField = fromPGSFromField

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




---------------------- defaults




defaultMultipartite :: MultiPartite
defaultMultipartite = MultiPartite a a
  where
    a = Partite HashSet.empty NgramsTerms


defaultGraph :: Graph
defaultGraph = Graph {_graph_nodes = [Node {node_x_coord=0, node_y_coord=0, node_size = 4, node_type = NgramsTerms, node_id = pack "0", node_label = pack "animal", node_attributes = Attributes {clust_default = 0}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 3, node_type = NgramsTerms, node_id = pack "1", node_label = pack "bird", node_attributes = Attributes {clust_default = 0}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = NgramsTerms, node_id = pack "2", node_label = pack "boy", node_attributes = Attributes {clust_default = 1}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = NgramsTerms, node_id = pack "3", node_label = pack "dog", node_attributes = Attributes {clust_default = 0}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = NgramsTerms, node_id = pack "4", node_label = pack "girl", node_attributes = Attributes {clust_default = 1}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 4, node_type = NgramsTerms, node_id = pack "5", node_label = pack "human body", node_attributes = Attributes {clust_default = 1}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 3, node_type = NgramsTerms, node_id = pack "6", node_label = pack "object", node_attributes = Attributes {clust_default = 2}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = NgramsTerms, node_id = pack "7", node_label = pack "pen", node_attributes = Attributes {clust_default = 2}, node_children = []},Node {node_x_coord=0, node_y_coord=0, node_size = 2, node_type = NgramsTerms, node_id = pack "8", node_label = pack "table", node_attributes = Attributes {clust_default = 2}, node_children = []}], _graph_edges = [Edge {edge_source = pack "0", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "0", ..},Edge {edge_source = pack "1", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "1", ..},Edge {edge_source = pack "1", edge_target = pack "1", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "2", ..},Edge {edge_source = pack "2", edge_target = pack "2", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "3", ..},Edge {edge_source = pack "2", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "4", ..},Edge {edge_source = pack "3", edge_target = pack "0", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "5", ..},Edge {edge_source = pack "3", edge_target = pack "1", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "6", ..},Edge {edge_source = pack "3", edge_target = pack "3", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "7", ..},Edge {edge_source = pack "4", edge_target = pack "4", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "8", ..},Edge {edge_source = pack "4", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "9", ..},Edge {edge_source = pack "5", edge_target = pack "5", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "10", ..},Edge {edge_source = pack "6", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "11", ..},Edge {edge_source = pack "7", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "12", ..},Edge {edge_source = pack "7", edge_target = pack "7", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "13", ..},Edge {edge_source = pack "8", edge_target = pack "6", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "14", ..},Edge {edge_source = pack "8", edge_target = pack "7", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "15", ..},Edge {edge_source = pack "8", edge_target = pack "8", edge_weight = 1.0, edge_confluence=0.5, edge_id = pack "16", ..}], _graph_metadata = Nothing}
  where
    edge_hidden = Just False



defaultHyperdataGraph :: HyperdataGraph
defaultHyperdataGraph = HyperdataGraph Nothing Nothing



-- | Intances for the mock
instance Arbitrary Strength where
  arbitrary = elements $ [Strong, Weak]
instance Arbitrary Graph where
  arbitrary = elements $ [defaultGraph]
