{-|
Module      : Gargantext.Database.Triggers
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams by node enable contextual metrics.

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Triggers
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS

------------------------------------------------------------------------

type MasterListId = ListId

insertOccsUpdates :: UserCorpusId -> MasterListId -> Cmd err [DPS.Only Int]
insertOccsUpdates cId lId = runPGSQuery query (cId, lId, nodeTypeId NodeList, nodeTypeId NodeDocument)
  where
    query :: DPS.Query
    query = [sql|
              INSERT INTO node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                SELECT nn.node1_id, lists.id, nnn.ngrams_id, 1, count(*) as c  -- type of score
                   FROM node_node_ngrams nnn
                   INNER JOIN nodes_nodes nn    ON nn.node2_id = nnn.node2_id
                   INNER JOIN nodes       docs  ON docs.id     = nnn.node2_id
                   INNER JOIN nodes       lists ON lists.parent_id = nn.node1_id
                  -- WHERE nn.node1_id  = NEW.node1_id -- .node1_id -- corpus_id
                  WHERE nn.node1_id  = ? -- .node1_id -- corpus_id
                    AND nnn.node1_id in (?, lists.id)  -- (masterList_id, userLists)
                    AND lists.typename = ?
                    AND docs.typename  = ?
                  GROUP BY nn.node1_id, lists.id, nnn.ngrams_id
               ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                  DO UPDATE SET weight = 3 -- c -- excluded.weight
                  RETURNING 1
                  -- TOCHECK
  |]



triggerOccsUpdates :: CorpusId -> ListId -> Cmd err [DPS.Only Int]
triggerOccsUpdates cId lId = runPGSQuery query (cId, lId, nodeTypeId NodeList, nodeTypeId NodeDocument)
  where
    query :: DPS.Query
    query = [sql|
                drop trigger trigger_occs on nodes_nodes;
                CREATE OR REPLACE FUNCTION occs_update() RETURNS trigger AS
                  $$
                      BEGIN
                      IF TG_OP = 'UPDATE' THEN
                      INSERT INTO node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                      -- TODO edge_type instead of ngrams_type
                        SELECT nn.node1_id, lists.id, nnn.ngrams_id, count(*), 1  -- type of score
                           FROM node_node_ngrams nnn
                           INNER JOIN nodes_nodes nn    ON nn.node2_id = nnn.node2_id
                           INNER JOIN nodes       docs  ON docs.id     = nnn.node2_id
                           INNER JOIN nodes       lists ON lists.parent_id = nn.node1_id
                          -- WHERE nn.node1_id  = NEW.node1_id -- .node1_id -- corpus_id
                          WHERE nn.node1_id  = ? -- .node1_id -- corpus_id
                            AND nnn.node1_id in (?, lists.id)  -- (masterList_id, userLists)
                            AND lists.typename = ?
                            AND docs.typename  = ?
                          GROUP BY nn.node1_id, lists.id, nnn.ngrams_id
                       ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                          DO UPDATE SET weight = excluded.weight;
                      END IF;
                    RETURN NULL;
                END $$
                LANGUAGE plpgsql;

                CREATE TRIGGER trigger_occs
                  AFTER UPDATE ON nodes_nodes
                  REFERENCING NEW TABLE AS NEW
                  FOR EACH STATEMENT
                  EXECUTE PROCEDURE occs_update();

                update nodes_nodes SET node1_id = node1_id;
  |]

