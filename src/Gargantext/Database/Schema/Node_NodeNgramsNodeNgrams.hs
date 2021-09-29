{-|
Module      : Gargantext.Database.Schema.Node_NodeNgrams_NodeNgrams
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

lgrams: listed ngrams

Node_NodeNgrams_NodeNgrams table is used to group ngrams
- first NodeId :: Referential / space node (corpus)
- NodeNgrams where Node is List
    - lgrams1_id, lgrams2_id where all lgrams2_id will be added to lgrams1_id
- weight: score the relation

Next Step benchmark:
- recursive queries of postgres
- group with: https://en.wikipedia.org/wiki/Nested_set_model

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.Node_NodeNgramsNodeNgrams
  where

import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Admin.Types.Node (CorpusId)
import Gargantext.Database.Schema.Node()
import Gargantext.Prelude

data Node_NodeNgrams_NodeNgrams_Poly node_id nng1_id nng2_id weight =
  Node_NodeNgrams_NodeNgrams { _nnn_node_id :: !node_id
                             , _nnn_nng1_id :: !nng1_id
                             , _nnn_nng2_id :: !nng2_id
                             , _nnn_weight  :: !weight
                             } deriving (Show)

type Node_NodeNgrams_NodeNgrams_Write =
  Node_NodeNgrams_NodeNgrams_Poly
    (Column PGInt4          )
    (Maybe (Column PGInt4  ))
    (Column PGInt4          )
    (Maybe (Column PGFloat8))

type Node_NodeNgrams_NodeNgrams_Read  =
  Node_NodeNgrams_NodeNgrams_Poly
    (Column PGInt4  )
    (Column PGInt4  )
    (Column PGInt4  )
    (Column PGFloat8)

type ListNgramsId = Int

type Node_NodeNgrams_NodeNgrams =
  Node_NodeNgrams_NodeNgrams_Poly CorpusId (Maybe ListNgramsId) ListNgramsId (Maybe Double)

$(makeAdaptorAndInstance "pNode_NodeNgrams_NodeNgrams"
                         ''Node_NodeNgrams_NodeNgrams_Poly)
$(makeLensesWith abbreviatedFields
                         ''Node_NodeNgrams_NodeNgrams_Poly)


node_NodeNgrams_NodeNgrams_Table :: Table Node_NodeNgrams_NodeNgrams_Write Node_NodeNgrams_NodeNgrams_Read
node_NodeNgrams_NodeNgrams_Table =
  Table "node_nodengrams_nodengrams"
       ( pNode_NodeNgrams_NodeNgrams Node_NodeNgrams_NodeNgrams
                       { _nnn_node_id = requiredTableField "node_id"
                       , _nnn_nng1_id = optionalTableField "node_ngrams1_id"
                       , _nnn_nng2_id = requiredTableField "node_ngrams2_id"
                       , _nnn_weight  = optionalTableField "weight"
                       }
       )

instance DefaultFromField PGInt4 (Maybe Int) where
    defaultFromField = fieldQueryRunnerColumn

instance DefaultFromField PGFloat8 (Maybe Double) where
    defaultFromField = fieldQueryRunnerColumn

