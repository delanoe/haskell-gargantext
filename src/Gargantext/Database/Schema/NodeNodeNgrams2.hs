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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNodeNgrams2
  where

import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Schema.NodeNgrams (NodeNgramsId)
import Gargantext.Database.Admin.Types.Node
import Prelude

data NodeNodeNgrams2Poly node_id nodengrams_id w
   = NodeNodeNgrams2 { _nnng2_node_id       :: !node_id
                     , _nnng2_nodengrams_id :: !nodengrams_id
                     , _nnng2_weight        :: !w
                     } deriving (Show)

type NodeNodeNgrams2Write =
     NodeNodeNgrams2Poly (Column PGInt4  )
                         (Column PGInt4  )
                         (Column PGFloat8)

type NodeNodeNgrams2Read  =
     NodeNodeNgrams2Poly (Column PGInt4  )
                         (Column PGInt4  )
                         (Column PGFloat8)

type NodeNodeNgrams2ReadNull =
     NodeNodeNgrams2Poly (Column (Nullable PGInt4  ))
                         (Column (Nullable PGInt4  ))
                         (Column (Nullable PGFloat8))

type NodeNodeNgrams2 =
  NodeNodeNgrams2Poly DocId NodeNgramsId Double

$(makeAdaptorAndInstance "pNodeNodeNgrams2" ''NodeNodeNgrams2Poly)
makeLenses ''NodeNodeNgrams2Poly

nodeNodeNgrams2Table :: Table NodeNodeNgrams2Write NodeNodeNgrams2Read
nodeNodeNgrams2Table  = Table "node_node_ngrams2"
                          ( pNodeNodeNgrams2 NodeNodeNgrams2
                               { _nnng2_node_id        = required "node_id"
                               , _nnng2_nodengrams_id  = required "nodengrams_id"
                               , _nnng2_weight         = required "weight"
                               }
                          )

