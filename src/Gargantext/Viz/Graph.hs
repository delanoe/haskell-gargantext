{-|
Module      : Gargantext.Viz.Graph
Description : 
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

import GHC.IO (FilePath)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson as DA

import Data.ByteString.Lazy as DBL (readFile, writeFile)

import Data.Text (Text)
import qualified Text.Read as T
import qualified Data.Text as T

import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix)


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


