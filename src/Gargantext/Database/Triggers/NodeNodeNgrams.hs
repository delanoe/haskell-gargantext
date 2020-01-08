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

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Triggers.NodeNodeNgrams
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils (Cmd, execPGSQuery)
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


