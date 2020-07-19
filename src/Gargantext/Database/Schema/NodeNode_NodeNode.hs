{-|
Module      : Gargantext.Database.Schema.NodeNode_NodeNode
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNode where

import Data.Maybe (Maybe)
import Gargantext.Core.Types
import Gargantext.Database.Schema.Prelude
import Gargantext.Prelude


data NodeNode_NodeNodePoly nn1 nn2 weight
  = NodeNode_NodeNode { _nnnn_nn1_id   :: !nn1
                      , _nnnn_nn2_id   :: !nn2
                      , _nnnn_weight   :: !weight
                      } deriving (Show)

type NodeNodeWrite     = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGFloat8)))

type NodeNodeRead      = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGFloat8)))

type NodeNodeReadNull  = NodeNodePoly (Column (Nullable PGInt4))
                                      (Column (Nullable PGInt4))
                                      (Column (Nullable PGFloat8))

type NodeNode_NodeNode = NodeNode_NodeNodePoly Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNode_NodeNode" ''NodeNode_NodeNodePoly)
makeLenses ''NodeNode_NodeNodePoly

nodeNode_NodeNodeTable :: Table NodeNode_NodeNodeWrite NodeNode_NodeNodeRead
nodeNode_NodeNodeTable  =
  Table "nodesnodes_nodesnodes"
        ( pNodeNode_NodeNode
          NodeNode_NodeNode { _nnnn_nn1_id = required "nn1_id"
                            , _nnnn_nn2_id = required "nn2_id"
                            , _nnnn_weight = optional "weight"
                            }
        )
