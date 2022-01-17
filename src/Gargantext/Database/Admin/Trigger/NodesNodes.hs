{-|
Module      : Gargantext.Database.Triggers.NodesNodes
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Triggers on NodesNodes table.

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Admin.Trigger.NodesNodes
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (Cmd, execPGSQuery)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS

type MasterListId = ListId

triggerInsertCount :: MasterListId -> Cmd err Int64
triggerInsertCount lId = execPGSQuery query (lId, nodeTypeId NodeList)
  where
    query :: DPS.Query
    query = [sql|
      CREATE OR REPLACE FUNCTION set_insert_count() RETURNS trigger
        AS $$
          BEGIN
                INSERT INTO node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                SELECT new1.node_id, lists.id, nnn.ngrams_id, nnn.ngrams_type, count(*) as weight
                FROM NEW as new1
                    INNER JOIN contexts            doc ON doc.id          = new1.context_id
                    INNER JOIN nodes             lists ON lists.parent_id = new1.node_id
                    INNER JOIN context_node_ngrams nnn ON nnn.context_id  = doc.id
                    WHERE lists.id in (?, lists.id)
                      AND lists.typename = ?
                    GROUP BY new1.node_id, lists.id, nnn.ngrams_id, nnn.ngrams_type
                ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                   DO UPDATE set weight = node_node_ngrams.weight + excluded.weight
                    ;
            RETURN NULL;
          END
        $$ LANGUAGE plpgsql;

        DROP   TRIGGER IF EXISTS trigger_insert_count              ON nodes_contexts;
        CREATE TRIGGER           trigger_insert_count AFTER INSERT ON nodes_contexts
        REFERENCING NEW TABLE AS NEW
        FOR EACH STATEMENT
        EXECUTE PROCEDURE set_insert_count();
  |]


triggerUpdateAdd :: MasterListId -> Cmd err Int64
triggerUpdateAdd lId = execPGSQuery query (lId, nodeTypeId NodeList)
  where
    query :: DPS.Query
    query = [sql|
       CREATE OR REPLACE FUNCTION set_update_ngrams_add() RETURNS trigger AS $$
        BEGIN
               UPDATE node_node_ngrams nnn0 SET weight = weight + d.fix_count
               FROM ( SELECT lists.parent_id AS node1_id
                    , lists.id               AS node2_id
                    , cnn.ngrams_id          AS ngrams_id
                    , cnn.ngrams_type        AS ngrams_type
                    , count(*)               AS fix_count
                       FROM NEW as new1
                       INNER JOIN contexts    doc         ON doc.id         = new1.context_id
                       INNER JOIN nodes       lists       ON new1.node_id   = lists.parent_id
                       INNER JOIN context_node_ngrams cnn ON cnn.context_id = doc.id
                      WHERE lists.id in (?, lists.id)  -- (masterList_id, userLists)
                        AND lists.typename = ?
                      GROUP BY node1_id, node2_id, ngrams_id, ngrams_type
                    ) as d
                WHERE nnn0.node1_id    = d.node1_id
                AND   nnn0.node2_id    = d.node2_id
                AND   nnn0.ngrams_id   = d.ngrams_id
                AND   nnn0.ngrams_type = d.ngrams_type
                ;
          RETURN NULL;
        END
        $$ LANGUAGE plpgsql;

        DROP trigger IF EXISTS trigger_count_update_add on nodes_contexts;
        CREATE TRIGGER trigger_count_update_add AFTER UPDATE on nodes_contexts
        REFERENCING OLD TABLE AS OLD
                    NEW TABLE AS NEW
        FOR EACH ROW
        WHEN (OLD.category <= 0 AND NEW.category >= 1)
        EXECUTE PROCEDURE set_update_ngrams_add();
  |]


triggerUpdateDel :: MasterListId -> Cmd err Int64
triggerUpdateDel lId = execPGSQuery query (lId, nodeTypeId NodeList)
  where
    query :: DPS.Query
    query = [sql|
       CREATE OR REPLACE FUNCTION set_update_ngrams_count_del() RETURNS trigger AS $$
        BEGIN
               UPDATE node_node_ngrams nnn0 SET weight = weight - d.fix_count
               FROM ( SELECT lists.parent_id AS node1_id
                    , lists.id               AS node2_id
                    , cnn.ngrams_id          AS ngrams_id
                    , cnn.ngrams_type        AS ngrams_type
                    , count(*)               AS fix_count
                         FROM NEW AS new1
                         INNER JOIN contexts    doc         ON doc.id          = new1.context_id
                         INNER JOIN nodes       lists       ON new1.node_id    = lists.parent_id
                         INNER JOIN context_node_ngrams cnn ON cnn.context_id  = doc.id
                        WHERE lists.id in (?, lists.id)  -- (masterList_id, userLists)
                          AND lists.typename = ?
                        GROUP BY node1_id, node2_id, ngrams_id, ngrams_type
                    ) as d
                WHERE nnn0.node1_id    = d.node1_id
                AND   nnn0.node2_id    = d.node2_id
                AND   nnn0.ngrams_id   = d.ngrams_id
                AND   nnn0.ngrams_type = d.ngrams_type
                ;
          RETURN NULL;
        END
        $$ LANGUAGE plpgsql;

        DROP   TRIGGER IF EXISTS trigger_count_delete2              ON nodes_contexts;
        CREATE TRIGGER           trigger_count_delete2 AFTER UPDATE ON nodes_contexts
        REFERENCING OLD TABLE AS OLD
                    NEW TABLE AS NEW
        FOR EACH ROW
        WHEN (OLD.category >= 1 AND NEW.category <= 0)
        EXECUTE PROCEDURE set_update_ngrams_count_del();
  |]




triggerDeleteCount :: MasterListId -> Cmd err Int64
triggerDeleteCount lId = execPGSQuery query (lId, toDBid NodeList)
  where
    query :: DPS.Query
    query = [sql|
      CREATE OR REPLACE FUNCTION set_delete_count() RETURNS trigger AS $$
      BEGIN
            UPDATE context_node_ngrams SET weight = weight - d.delete_count
             FROM ( SELECT lists.id as node_id
                  , old1.context_id as context_id
                  , nnn.ngrams_id as ngrams_id
                  , nnn.ngrams_type as ngrams_type
                  , count(*) as delete_count FROM OLD as old1
                      INNER JOIN contexts           doc    ON doc.id          = old1.context_id
                      INNER JOIN nodes              lists  ON lists.parent_id = old1.node_id
                      INNER JOIN context_node_ngrams nnn   ON nnn.context_id  = doc.id
                      WHERE nnn.node_id in (?, lists.id)
                        AND lists.typename = ?
                      GROUP BY old1.context_id, lists.id, nnn.ngrams_id, nnn.ngrams_type
                  ) AS d
            WHERE context_node_ngrams.context_id  = d.context_id
              AND context_node_ngrams.node_id     = d.node_id
              AND context_node_ngrams.ngrams_id   = d.ngrams_id
              AND context_node_ngrams.ngrams_type = d.ngrams_type
                ;
        RETURN NULL;
      END
      $$ LANGUAGE plpgsql;

      -- DROP trigger trigger_delete_count on nodes_nodes;
      CREATE TRIGGER trigger_delete_count AFTER DELETE on nodes_contexts
      REFERENCING OLD TABLE AS OLD
      FOR EACH STATEMENT
      EXECUTE PROCEDURE set_delete_count();
  |]


-- TODO add groups
{-
triggerCoocInsert :: MasterListId -> Cmd err Int64
triggerCoocInsert lid = execPGSQuery query ( lid
                                           -- , nodeTypeId NodeCorpus
                                           -- , nodeTypeId NodeDocument
                                           -- , nodeTypeId NodeList
                                           , toDBid CandidateTerm
                                           , toDBid CandidateTerm
                                           )
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION nodes_nodes_set_cooc() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_nodengrams_nodengrams (node_id, node_ngrams1_id, node_ngrams2_id, weight)
                WITH input(corpus_id, nn1, nn2, weight) AS (
                  SELECT new1.node1_id, nn1.id, nn2.id, count(*) from NEW as new1
                        INNER JOIN node_ngrams nn1
                                ON nn1.node_id = ? -- COALESCE(?,?) --(masterList, userList)
                        INNER JOIN node_ngrams nn2
                                ON nn2.node_id = nn1.node_id

                        INNER JOIN node_node_ngrams2 nnn1
                                ON nnn1.node_id = new1.node2_id

                        INNER JOIN node_node_ngrams2 nnn2
                                ON nnn2.node_id = new1.node2_id

                        WHERE nnn1.nodengrams_id = nn1.id
                          AND nnn2.nodengrams_id = nn2.id
                          AND nn1.id < nn2.id
                          AND nn1.node_subtype >= ?
                          AND nn2.node_subtype >= ?
                        GROUP BY new1.node1_id, nn1.id, nn2.id
                        )
                    SELECT * from input where weight >= 1

                ON CONFLICT (node_id, node_ngrams1_id, node_ngrams2_id)
                   DO UPDATE set weight = node_nodengrams_nodengrams.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          -- DROP trigger trigger_cooc on node_node_ngrams2;

          CREATE TRIGGER trigger_cooc_insert AFTER INSERT on nodes_nodes
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE nodes_nodes_set_cooc();
   |]
-}
