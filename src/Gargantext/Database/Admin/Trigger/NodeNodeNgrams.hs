{-|
Module      : Gargantext.Database.Triggers.NodeNodeNgrams
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Triggers on NodeNodeNgrams table.

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Admin.Trigger.NodeNodeNgrams
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core.Types.Main (listTypeId, ListType(CandidateTerm))
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (Cmd, execPGSQuery)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS

triggerCountInsert :: Cmd err Int64
triggerCountInsert = execPGSQuery query (nodeTypeId NodeDocument, nodeTypeId NodeList)
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_ngrams_global_count() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                select n.parent_id, n.id, new1.ngrams_id, new1.ngrams_type, count(*) from NEW as new1
                    INNER JOIN nodes n  ON n.id = new1.node1_id
                    INNER JOIN nodes n2 ON n2.id = new1.node2_id
                    WHERE n2.typename = ?  -- not mandatory
                      AND n.typename = ?   -- not mandatory
                      AND n.parent_id <> n2.id -- not mandatory
                    GROUP BY n.parent_id, n.id, new1.ngrams_id, new1.ngrams_type
                ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                   DO UPDATE set weight = node_node_ngrams.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          -- DROP trigger trigger_count_insert on node_node_ngrams;

          CREATE TRIGGER trigger_count_insert AFTER INSERT on node_node_ngrams
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_ngrams_global_count();
   |]

triggerCountInsert2 :: Cmd err Int64
triggerCountInsert2 = execPGSQuery query ( nodeTypeId NodeCorpus
                                         , nodeTypeId NodeDocument
                                         , nodeTypeId NodeList
                                         )
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_ngrams_global_count2() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_node_ngrams2 (node_id, nodengrams_id, weight)
                  SELECT corpus.id, nng.id, count(*) from NEW as new1
                        INNER JOIN node_ngrams nng    ON nng.id = new1.nodengrams_id
                        INNER JOIN nodes       list   ON list.id   = nng.node_id
                        INNER JOIN nodes_nodes nn     ON nn.node2_id = new1.node_id
                        INNER JOIN nodes       corpus ON corpus.id   = nn.node1_id
                        INNER JOIN nodes       doc    ON doc.id      = nn.node2_id
                        WHERE corpus.typename = ? -- 30 -- corpus
                          AND doc.typename    = ? -- 4  -- maybe not mandatory
                          AND list.typename   = ? -- 5  -- list
                        GROUP BY corpus.id, nng.id

                ON CONFLICT (node_id, nodengrams_id)
                   DO UPDATE set weight = node_node_ngrams2.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          -- DROP trigger trigger_count_insert2 on node_node_ngrams2;

          CREATE TRIGGER trigger_count_insert2 AFTER INSERT on node_node_ngrams2
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_ngrams_global_count2();
   |]

-- TODO add the groups
triggerCoocInsert :: Cmd err Int64
triggerCoocInsert = execPGSQuery query ( nodeTypeId NodeCorpus
                                       , nodeTypeId NodeDocument
                                       , nodeTypeId NodeList
                                       , listTypeId CandidateTerm
                                       , listTypeId CandidateTerm
                                       )
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_cooc() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_nodengrams_nodengrams (node_id, node_ngrams1_id, node_ngrams2_id, weight)
                WITH input(corpus_id, nn1, nn2, weight) AS (
                  SELECT corpus.id, nng1.id, nng2.id, count(*) from NEW as new1
                        INNER JOIN node_ngrams nng1   ON nng1.id     = new1.nodengrams_id
                        INNER JOIN nodes       list   ON list.id     = nng1.node_id
                        INNER JOIN nodes_nodes nn     ON nn.node2_id = new1.node_id
                        INNER JOIN nodes       corpus ON corpus.id   = nn.node1_id
                        INNER JOIN nodes       doc    ON doc.id      = nn.node2_id

                        INNER JOIN node_node_ngrams2 nnng2 ON nnng2.node_id  = doc.id
                        INNER JOIN node_ngrams       nng2  ON nng2.id        = nnng2.nodengrams_id

                        WHERE corpus.typename = ? -- 30 -- corpus
                          AND doc.typename    = ? -- 4  -- maybe not mandatory
                          AND list.typename   = ? -- 5  -- list
                          AND nng2.node_id    = list.id
                          AND nng1.id < nng2.id
                          AND nng1.node_subtype >= ?
                          AND nng2.node_subtype >= ?
                        GROUP BY corpus.id, nng1.id, nng2.id
                        )
                    SELECT * from input where weight > 1

                ON CONFLICT (node_id, node_ngrams1_id, node_ngrams2_id)
                   DO UPDATE set weight = node_nodengrams_nodengrams.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          -- DROP trigger trigger_cooc on node_node_ngrams2;

          CREATE TRIGGER trigger_cooc_insert AFTER INSERT on node_node_ngrams2
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_cooc();
   |]

