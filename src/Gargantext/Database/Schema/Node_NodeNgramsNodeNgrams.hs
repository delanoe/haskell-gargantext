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

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Database.Schema.Node_NodeNgramsNodeNgrams
  where

import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Admin.Utils (Cmd, runOpaQuery, mkCmd)
import Gargantext.Database.Admin.Types.Node (CorpusId, pgNodeId)
import Gargantext.Database.Schema.Node()
import Gargantext.Prelude
import Opaleye

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
                       { _nnn_node_id = required "node_id"
                       , _nnn_nng1_id = optional "node_ngrams1_id"
                       , _nnn_nng2_id = required "node_ngrams2_id"
                       , _nnn_weight  = optional "weight"
                       }
       )

queryNode_NodeNgrams_NodeNgrams_Table :: Query Node_NodeNgrams_NodeNgrams_Read
queryNode_NodeNgrams_NodeNgrams_Table = queryTable node_NodeNgrams_NodeNgrams_Table

-- | Select NodeNgramsNgrams
-- TODO not optimized (get all ngrams without filters)
node_Node_NodeNgrams_NodeNgrams :: Cmd err [Node_NodeNgrams_NodeNgrams]
node_Node_NodeNgrams_NodeNgrams = runOpaQuery queryNode_NodeNgrams_NodeNgrams_Table

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


-- TODO: Add option on conflict
insert_Node_NodeNgrams_NodeNgrams :: [Node_NodeNgrams_NodeNgrams] -> Cmd err Int64
insert_Node_NodeNgrams_NodeNgrams = insert_Node_NodeNgrams_NodeNgrams_W
                 . map (\(Node_NodeNgrams_NodeNgrams n ng1 ng2 maybeWeight) ->
                          Node_NodeNgrams_NodeNgrams (pgNodeId n  )
                                           (pgInt4 <$> ng1)
                                           (pgInt4 ng2)
                                           (pgDouble <$> maybeWeight)
                        )

insert_Node_NodeNgrams_NodeNgrams_W :: [Node_NodeNgrams_NodeNgrams_Write] -> Cmd err Int64
insert_Node_NodeNgrams_NodeNgrams_W ns =
  mkCmd $ \c -> runInsert_ c Insert { iTable = node_NodeNgrams_NodeNgrams_Table
                                    , iRows  = ns
                                    , iReturning = rCount
                                    , iOnConflict = (Just DoNothing)
                                    }
