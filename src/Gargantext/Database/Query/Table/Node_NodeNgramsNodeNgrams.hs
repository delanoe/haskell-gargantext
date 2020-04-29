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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.Node_NodeNgramsNodeNgrams
  ( module Gargantext.Database.Schema.Node_NodeNgramsNodeNgrams
  , insert_Node_NodeNgrams_NodeNgrams
  )
  where

import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Prelude (Cmd, runOpaQuery, mkCmd)
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Schema.Node_NodeNgramsNodeNgrams
import Gargantext.Database.Schema.Node()
import Gargantext.Prelude


queryNode_NodeNgrams_NodeNgrams_Table :: Query Node_NodeNgrams_NodeNgrams_Read
queryNode_NodeNgrams_NodeNgrams_Table = queryTable node_NodeNgrams_NodeNgrams_Table

-- | Select NodeNgramsNgrams
-- TODO not optimized (get all ngrams without filters)
_node_Node_NodeNgrams_NodeNgrams :: Cmd err [Node_NodeNgrams_NodeNgrams]
_node_Node_NodeNgrams_NodeNgrams = runOpaQuery queryNode_NodeNgrams_NodeNgrams_Table


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
