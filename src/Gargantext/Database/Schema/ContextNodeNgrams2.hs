{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.ContextNodeNgrams2
  where

import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.NodeNgrams (NodeNgramsId)
import Gargantext.Database.Schema.Prelude
import Prelude


type ContextNodeNgrams2 = ContextNodeNgrams2Poly ContextId NodeNgramsId Double


data ContextNodeNgrams2Poly context_id nodengrams_id w
   = ContextNodeNgrams2 { _cnng2_context_id    :: !context_id
                        , _cnng2_nodengrams_id :: !nodengrams_id
                        , _cnng2_weight        :: !w
                       } deriving (Show)

type ContextNodeNgrams2Write =
     ContextNodeNgrams2Poly (Column SqlInt4  )
                            (Column SqlInt4  )
                            (Column SqlFloat8)

type ContextNodeNgrams2Read  =
     ContextNodeNgrams2Poly (Column SqlInt4  )
                            (Column SqlInt4  )
                            (Column SqlFloat8)

$(makeAdaptorAndInstance "pContextNodeNgrams2" ''ContextNodeNgrams2Poly)
makeLenses ''ContextNodeNgrams2Poly

contextNodeNgrams2Table :: Table ContextNodeNgrams2Write ContextNodeNgrams2Read
contextNodeNgrams2Table  = Table "context_node_ngrams2"
                          ( pContextNodeNgrams2 ContextNodeNgrams2
                               { _cnng2_context_id     = requiredTableField "context_id"
                               , _cnng2_nodengrams_id  = requiredTableField "nodengrams_id"
                               , _cnng2_weight         = requiredTableField "weight"
                               }
                          )
