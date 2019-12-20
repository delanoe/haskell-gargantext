{-|
Module      : Gargantext.Database.Triggers.Nodes
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Triggers on Nodes table.

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Triggers.Nodes
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils (Cmd, execPGSQuery)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS


triggerSearchUpdate :: Cmd err Int64
triggerSearchUpdate = execPGSQuery query ( nodeTypeId NodeDocument
                                         , nodeTypeId NodeDocument
                                         , nodeTypeId NodeContact
                                         )
  where
    query :: DPS.Query
    query = [sql|
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
            new.search := to_tsvector( 'english' , new.name);
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

