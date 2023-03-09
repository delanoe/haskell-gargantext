{-|
Module      : Gargantext.Database.Select.Table.NodeContext_NodeContext
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeContext_NodeContext
  ( module Gargantext.Database.Schema.NodeContext_NodeContext
--  , query_NodeContext_NodeContext_Table
  , insertNodeContext_NodeContext
  )
  where

import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.NodeContext_NodeContext
import Gargantext.Database.Schema.Prelude hiding (sum)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as PGS

{-
queryNodeContext_NodeContextTable :: Select NodeContext_NodeContextRead
queryNodeContext_NodeContextTable = selectTable nodeContext_NodeContextTable
-}

insertNodeContext_NodeContext :: [(CorpusId, DocId, AnnuaireId, ContactId)] -> Cmd err [Int]
insertNodeContext_NodeContext contexts = do
  let
    fields = map (\t -> QualifiedIdentifier Nothing t) $ snd fields_name
    fields_name :: ( [Text], [Text])
    fields_name = ( ["corpus_id", "doc_id", "annuaire_id", "contact_id"]
                            , ["int4"     , "int4"  , "int4"       , "int4"      ]
                            )
  result <- map (\(PGS.Only a) -> a) <$> runPGSQuery queryInsert (PGS.Only $ Values fields contexts)
  pure [sum result]

queryInsert :: PGS.Query
queryInsert = [sql|
  WITH input(corpus_id, doc_id, annuaire_id, contact_id) AS (?)
  INSERT into nodescontexts_nodescontexts (nodescontexts1, nodescontexts2)
  SELECT context1.id, context2.id FROM input
  INNER JOIN nodes_contexts context1 ON context1.node_id = input.corpus_id
  INNER JOIN nodes_contexts context2 ON context2.node_id = input.annuaire_id
  WHERE context1.context_id = input.doc_id
  AND   context2.context_id = input.contact_id
  ON CONFLICT (nodescontexts1, nodescontexts2) DO Nothing
  RETURNING 1
  |]
