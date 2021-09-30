{-|
Module      : Gargantext.Database.Schema.NodeNode
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNode where

import Gargantext.Core.Types
import Gargantext.Database.Schema.Prelude
import Gargantext.Prelude


data NodeNodePoly node1_id node2_id score cat
                   = NodeNode { _nn_node1_id   :: !node1_id
                              , _nn_node2_id   :: !node2_id
                              , _nn_score      :: !score
                              , _nn_category   :: !cat
                              } deriving (Show)

type NodeNodeWrite     = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGFloat8)))
                                      (Maybe  (Column (PGInt4)))

type NodeNodeRead      = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Column (PGFloat8))
                                      (Column (PGInt4))

type NodeNodeReadNull  = NodeNodePoly (Column (Nullable PGInt4))
                                      (Column (Nullable PGInt4))
                                      (Column (Nullable PGFloat8))
                                      (Column (Nullable PGInt4))

type NodeNode = NodeNodePoly NodeId NodeId (Maybe Double) (Maybe Int)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
makeLenses ''NodeNodePoly

nodeNodeTable :: Table NodeNodeWrite NodeNodeRead
nodeNodeTable  =
  Table "nodes_nodes"
         ( pNodeNode
           NodeNode { _nn_node1_id = requiredTableField "node1_id"
                    , _nn_node2_id = requiredTableField "node2_id"
                    , _nn_score    = optionalTableField "score"
                    , _nn_category = optionalTableField "category"
                    }
                )

instance DefaultFromField (Nullable PGInt4)   Int            where
    defaultFromField = fieldQueryRunnerColumn

instance DefaultFromField (Nullable PGFloat8) Int            where
    defaultFromField = fieldQueryRunnerColumn

instance DefaultFromField (Nullable PGFloat8) Double         where
    defaultFromField = fieldQueryRunnerColumn

instance DefaultFromField PGFloat8            (Maybe Double) where
    defaultFromField = fieldQueryRunnerColumn

instance DefaultFromField PGInt4              (Maybe Int)    where
    defaultFromField = fieldQueryRunnerColumn

