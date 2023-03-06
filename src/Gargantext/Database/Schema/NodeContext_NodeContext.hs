{-|
Module      : Gargantext.Database.Schema.ContextContext
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

module Gargantext.Database.Schema.NodeContext_NodeContext where

import Gargantext.Core.Types
import Gargantext.Database.Schema.Prelude
import Gargantext.Prelude



data NodeContext_NodeContextPoly nodecontext1 nodecontext2
                   = NodeContext_NodeContext { _ncnc_nodecontext1 :: !nodecontext1
                                             , _ncnc_nodecontext2 :: !nodecontext2
                                    } deriving (Show)

type NodeContext_NodeContextWrite = NodeContext_NodeContextPoly (Column (SqlInt4))
                                                                (Column (SqlInt4))

type NodeContext_NodeContextRead  = NodeContext_NodeContextPoly (Column (SqlInt4))
                                                                (Column (SqlInt4))

type NodeContext_NodeContext = NodeContext_NodeContextPoly NodeContextId NodeContextId

$(makeAdaptorAndInstance "pNodeContext_NodeContext" ''NodeContext_NodeContextPoly)
makeLenses ''NodeContext_NodeContextPoly

nodeContext_NodeContextTable :: Table NodeContext_NodeContextWrite NodeContext_NodeContextRead
nodeContext_NodeContextTable =
  Table "nodescontexts_nodescontexts"
         ( pNodeContext_NodeContext
           NodeContext_NodeContext { _ncnc_nodecontext1 = requiredTableField "nodescontexts1"
                                   , _ncnc_nodecontext2 = requiredTableField "nodescontexts2"
                                   }
          )
queryNodeContext_NodeContextTable :: Query NodeContext_NodeContextRead
queryNodeContext_NodeContextTable = selectTable nodeContext_NodeContextTable
