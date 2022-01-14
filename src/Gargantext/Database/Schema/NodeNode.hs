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

type NodeNodeWrite     = NodeNodePoly (Column (SqlInt4))
                                      (Column (SqlInt4))
                                      (Maybe  (Column (SqlFloat8)))
                                      (Maybe  (Column (SqlInt4)))

type NodeNodeRead      = NodeNodePoly (Column (SqlInt4))
                                      (Column (SqlInt4))
                                      (Column (SqlFloat8))
                                      (Column (SqlInt4))

type NodeNodeReadNull  = NodeNodePoly (Column (Nullable SqlInt4))
                                      (Column (Nullable SqlInt4))
                                      (Column (Nullable SqlFloat8))
                                      (Column (Nullable SqlInt4))

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

instance DefaultFromField (Nullable SqlInt4)   Int            where
    defaultFromField = fromPGSFromField

instance DefaultFromField (Nullable SqlFloat8) Int            where
    defaultFromField = fromPGSFromField

instance DefaultFromField (Nullable SqlFloat8) Double         where
    defaultFromField = fromPGSFromField

instance DefaultFromField SqlFloat8            (Maybe Double) where
    defaultFromField = fromPGSFromField

instance DefaultFromField SqlInt4              (Maybe Int)    where
    defaultFromField = fromPGSFromField

