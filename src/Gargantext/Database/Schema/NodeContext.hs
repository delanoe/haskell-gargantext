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

module Gargantext.Database.Schema.NodeContext where

import Gargantext.Core.Types
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Schema.NodeNode () -- Just importing some instances
import Gargantext.Prelude


data NodeContextPoly node_id context_id score cat
                   = NodeContext { _nc_node_id     :: !node_id
                                 , _nc_context_id  :: !context_id
                                 , _nc_score       :: !score
                                 , _nc_category    :: !cat
                                 } deriving (Show)

type NodeContextWrite     = NodeContextPoly (Column (SqlInt4))
                                      (Column (SqlInt4))
                                      (Maybe  (Column (SqlFloat8)))
                                      (Maybe  (Column (SqlInt4)))

type NodeContextRead      = NodeContextPoly (Column (SqlInt4))
                                      (Column (SqlInt4))
                                      (Column (SqlFloat8))
                                      (Column (SqlInt4))

type NodeContextReadNull  = NodeContextPoly (Column (Nullable SqlInt4))
                                      (Column (Nullable SqlInt4))
                                      (Column (Nullable SqlFloat8))
                                      (Column (Nullable SqlInt4))

type NodeContext = NodeContextPoly NodeId NodeId (Maybe Double) (Maybe Int)

$(makeAdaptorAndInstance "pNodeContext" ''NodeContextPoly)
makeLenses ''NodeContextPoly

nodeContextTable :: Table NodeContextWrite NodeContextRead
nodeContextTable  =
  Table "nodes_contexts"
         ( pNodeContext
           NodeContext { _nc_node_id    = requiredTableField "node_id"
                       , _nc_context_id = requiredTableField "context_id"
                       , _nc_score      = optionalTableField "score"
                       , _nc_category   = optionalTableField "category"
                       }
                   )
