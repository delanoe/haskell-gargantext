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

import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)

import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix)

-----------------------------------------------------------
data TypeNode = Terms | Unknown
  deriving (Show, Generic)

$(deriveJSON (unPrefix "") ''TypeNode)

data Attributes = Attributes { clust_default :: Int }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "") ''Attributes)

data Node = Node { n_size :: Int
                 , n_type :: TypeNode
                 , n_id   :: Text
                 , n_label :: Text
                 , n_attributes :: Attributes
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "n_") ''Node)

data Edge = Edge { e_source :: Int
                 , e_target :: Int
                 , e_weight :: Double
                 , e_id     :: Int
                 }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "e_") ''Edge)

data Graph = Graph { g_nodes :: [Node]
                   , g_edges :: [Edge]
                   }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "g_") ''Graph)

-----------------------------------------------------------



