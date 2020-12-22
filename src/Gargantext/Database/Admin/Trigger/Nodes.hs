{-|
Module      : Gargantext.Database.Admin.Trigger.Nodes
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Triggers on Nodes table.

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Admin.Trigger.Nodes
  where

import Data.Text (Text)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core (HasDBid(..))
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (Cmd, execPGSQuery)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS


triggerSearchUpdate :: HasDBid NodeType => Cmd err Int64
triggerSearchUpdate = execPGSQuery query ( hasDBid NodeDocument
                                         , hasDBid NodeDocument
                                         , hasDBid NodeContact
                                         )
  where
    query :: DPS.Query
    query = [sql|
        -- DROP TRIGGER search_update_trigger on nodes;
        CREATE OR REPLACE FUNCTION public.search_update()
        RETURNS trigger AS $$
        begin
          IF new.typename = ? AND new.hyperdata @> '{"language_iso2":"EN"}' THEN
            new.search := to_tsvector( 'english' , (new.hyperdata ->> 'title') || ' ' || (new.hyperdata ->> 'abstract'));

          ELSIF new.typename = ? AND new.hyperdata @> '{"language_iso2":"FR"}' THEN
            new.search := to_tsvector( 'french' , (new.hyperdata ->> 'title') || ' ' || (new.hyperdata ->> 'abstract'));

          ELSIF new.typename = ? THEN
            new.search := to_tsvector( 'french' , (new.hyperdata ->> 'prenom')
                                         || ' ' || (new.hyperdata ->> 'nom')
                                         || ' ' || (new.hyperdata ->> 'fonction')
                                     );
          ELSE
            new.search := to_tsvector( 'english' , (new.hyperdata ->> 'title') || ' ' || (new.hyperdata ->> 'abstract'));
          END IF;
          return new;
        end
        $$ LANGUAGE plpgsql;

        ALTER FUNCTION public.search_update() OWNER TO gargantua;

        CREATE TRIGGER search_update_trigger
          BEFORE INSERT OR UPDATE
          ON nodes FOR EACH ROW
          EXECUTE PROCEDURE search_update();

      -- Initialize index with already existing data
      UPDATE nodes SET hyperdata = hyperdata;

  |]

type Secret = Text

triggerUpdateHash :: HasDBid NodeType => Secret -> Cmd err Int64
triggerUpdateHash secret = execPGSQuery query ( hasDBid NodeDocument
                                              , hasDBid NodeContact
                                              , secret
                                              , secret
                                              , hasDBid NodeDocument
                                              , hasDBid NodeContact
                                              , secret
                                              , secret
                                              )
  where
    query :: DPS.Query
    query = [sql|

      CREATE OR REPLACE FUNCTION hash_insert_nodes()
      RETURNS trigger AS $$
      BEGIN
       IF NEW.hash_id = ''
         THEN
           IF NEW.typename = ? OR NEW.typename = ?
             THEN NEW.hash_id = digest(CONCAT(?, NEW.typename, NEW.name, NEW.parent_id, NEW.hyperdata), 'sha256');
             ELSE NEW.hash_id = digest(CONCAT(?, NEW.typename, NEW.name, NEW.id, NEW.hyperdata), 'sha256');
           END IF;
        END IF;
        RETURN NEW;
      END
      $$ LANGUAGE plpgsql;


      CREATE OR REPLACE FUNCTION hash_update_nodes()
      RETURNS trigger AS $$
      BEGIN
        IF NEW.typename = ? OR NEW.typename = ?
             THEN NEW.hash_id = digest(CONCAT(?, NEW.typename, NEW.name, NEW.parent_id, NEW.hyperdata), 'sha256');
             ELSE NEW.hash_id = digest(CONCAT(?, NEW.typename, NEW.name, NEW.id, NEW.hyperdata), 'sha256');
        END IF;
        RETURN NEW;
      END
      $$ LANGUAGE plpgsql;


      CREATE TRIGGER nodes_hash_insert BEFORE INSERT ON nodes FOR EACH ROW EXECUTE PROCEDURE hash_insert_nodes();
      CREATE TRIGGER nodes_hash_update BEFORE UPDATE ON nodes FOR EACH ROW EXECUTE PROCEDURE hash_update_nodes();

  |]


