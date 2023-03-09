{-|
Module      : Main.hs
Description : Gargantext Import Corpus
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Import a corpus binary.

 -}

{-# LANGUAGE Strict            #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where


import Data.Either (Either(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.IO.Exception (IOException)
import Gargantext.API.Admin.EnvTypes (DevEnv)
import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Node () -- instances only
import Gargantext.API.Prelude (GargError)
import Gargantext.API.Ngrams.Tools (migrateFromDirToDb)
import Gargantext.Core (HasDBid(toDBid))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (getOrMk_RootWithCorpus)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Trigger.Init
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataCorpus)
import Gargantext.Database.Admin.Types.Node (NodeType(NodeDocument, NodeContact))
import Gargantext.Database.Prelude (Cmd'', Cmd, execPGSQuery)
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import Prelude (getLine)
import System.Environment (getArgs)
import qualified Data.List as List (cycle, concat, take, unlines)

main :: IO ()
main = do

  let ___ = putStrLn
          $ List.concat
          $ List.take 72
          $ List.cycle ["_"]

  ___
  putStrLn "GarganText upgrade to version 0.0.6"
  ___

  params@[iniPath] <- getArgs
  _ <- if length params /= 1
         then panic "Usage: ./gargantext-upgrade gargantext.ini"
         else pure ()

  putStrLn $ List.unlines
           [ "Your Database defined in gargantext.ini will be upgraded."
           , "We stronlgy recommend you to make a backup using pg_dump."
           , ""
           , "Press ENTER if you want to continue, CTRL+C if you want to stop."
           ]

  _ok  <- getLine

  cfg       <- readConfig         iniPath
  let _secret = _gc_secretkey cfg

  withDevEnv iniPath $ \env -> do
    -- First upgrade the Database Schema
    _ <- runCmdDev env (migrateFromDirToDb :: Cmd GargError ())

    ___
    putStrLn "Uprade done with success !"
    ___
    pure ()


{-
sqlUpdateTriggerHash :: Cmd'' DevEnv IOException Int64
sqlUpdateTriggerHash = do
  execPGSQuery query ()
    where
      query = [sql|
      UPDATE nodes    SET typename = typename;
      UPDATE contexts SET typename = typename;
     |]


sqlNodes2Contexts :: Cmd'' DevEnv IOException Int64
sqlNodes2Contexts = do
  execPGSQuery query (toDBid NodeDocument,toDBid NodeContact)
    where
      query = [sql|
        -- WITH docs (id,hash_id,typename,user_id,parent_id,name,date,hyperdata, search)
        WITH docs AS (SELECT * from nodes  WHERE nodes.typename IN (?,?)),

        inserted (id, hash_id) AS (
          INSERT INTO contexts (hash_id,typename,user_id,parent_id,name,date,hyperdata, search)
            SELECT d.hash_id,d.typename,d.user_id,NULL,d.name,d.date,d.hyperdata,search FROM docs AS d
            RETURNING contexts.id, contexts.hash_id
         ),

        indexed (node_id, context_id) AS (
          SELECT docs.id, inserted.id from inserted
          JOIN docs on docs.hash_id = inserted.hash_id
        ),

        -- nodes_nodes -> nodes_contexts
        nodes_contexts_query AS (
        INSERT INTO nodes_contexts (node_id, context_id,score, category)
        SELECT nn.node1_id,i.context_id,nn.score,nn.category FROM nodes_nodes nn
        JOIN indexed i ON i.node_id = nn.node2_id
        ),

        -- nodes_nodes_ngrams -> contexts_nodes_ngrams
        contexts_nodes_ngrams_query AS (
        INSERT INTO context_node_ngrams
        SELECT i.context_id, nnn.node1_id, nnn.ngrams_id, nnn.ngrams_type, nnn.weight FROM node_node_ngrams nnn
        JOIN indexed i ON i.node_id = nnn.node2_id
        ),

        ---- nodes_nodes_ngrams2 -> contexts_nodes_ngrams2
        context_node_ngrams2_query AS (
        INSERT INTO context_node_ngrams2
        SELECT i.context_id, nnn2.nodengrams_id, nnn2.weight FROM node_node_ngrams2 nnn2
        JOIN indexed i ON i.node_id = nnn2.node_id
        )

        -- WITH CASCADE it should update others tables
        DELETE FROM nodes n
        USING indexed i WHERE i.node_id = n.id
        ;

        UPDATE contexts SET parent_id = id;


  |]





sqlSchema :: Cmd'' DevEnv IOException Int64
sqlSchema = do
  execPGSQuery query ()
    where
      query = [sql|
        -- TODO typename -> type_id
        CREATE TABLE public.contexts (
            id        SERIAL,
            hash_id   CHARACTER varying(66) DEFAULT ''::character varying NOT NULL,
            typename  INTEGER NOT NULL,
            user_id   INTEGER NOT NULL,
            parent_id INTEGER REFERENCES public.contexts(id) ON DELETE CASCADE ,
            name      CHARACTER varying(255) DEFAULT ''::character varying NOT NULL,
            date      TIMESTAMP with time zone DEFAULT now() NOT NULL,
            hyperdata jsonb DEFAULT '{}'::jsonb NOT NULL,
            search tsvector,
            PRIMARY KEY (id),
            FOREIGN KEY (user_id)  REFERENCES public.auth_user(id) ON DELETE CASCADE
        );
        ALTER TABLE public.contexts OWNER TO gargantua;



        -- To attach contexts to a Corpus
        CREATE TABLE public.nodes_contexts (
            node_id    INTEGER NOT NULL REFERENCES public.nodes(id)    ON DELETE CASCADE,
            context_id INTEGER NOT NULL REFERENCES public.contexts(id) ON DELETE CASCADE,
            score    REAL    ,
            category INTEGER ,
            PRIMARY KEY (node_id, context_id)
        );
        ALTER TABLE public.nodes_contexts OWNER TO gargantua;

         ---------------------------------------------------------------
        CREATE TABLE public.context_node_ngrams (
            context_id    INTEGER NOT NULL REFERENCES public.contexts (id) ON DELETE CASCADE,
            node_id       INTEGER NOT NULL REFERENCES public.nodes    (id) ON DELETE CASCADE,
            ngrams_id     INTEGER NOT NULL REFERENCES public.ngrams   (id) ON DELETE CASCADE,
            ngrams_type   INTEGER  ,
            weight double precision,
            PRIMARY KEY (context_id, node_id, ngrams_id, ngrams_type)
          );


        ALTER TABLE public.context_node_ngrams OWNER TO gargantua;

        CREATE TABLE public.context_node_ngrams2 (
            context_id      INTEGER NOT NULL REFERENCES public.contexts  (id)       ON DELETE CASCADE,
            nodengrams_id   INTEGER NOT NULL REFERENCES public.node_ngrams  (id) ON DELETE CASCADE,
            weight double   precision,
            PRIMARY KEY (context_id, nodengrams_id)
        );
        ALTER TABLE public.context_node_ngrams2 OWNER TO gargantua;



        CREATE INDEX        ON public.contexts USING gin (hyperdata);
        CREATE INDEX        ON public.contexts USING btree (user_id, typename, parent_id);
        CREATE INDEX        ON public.contexts USING btree (id, typename, date ASC);
        CREATE INDEX        ON public.contexts USING btree (id, typename, date DESC);
        CREATE INDEX        ON public.contexts USING btree (typename, id);
        CREATE UNIQUE INDEX ON public.contexts USING btree (hash_id);


        -- To make the links between Corpus Node and its contexts
        CREATE UNIQUE INDEX ON public.nodes_contexts  USING btree (node_id, context_id);
        CREATE INDEX        ON public.nodes_contexts  USING btree (node_id, context_id, category);


        ------------------------------------------------------------------------
        CREATE UNIQUE INDEX ON public.context_node_ngrams USING btree (context_id, node_id, ngrams_id, ngrams_type);
        CREATE        INDEX ON public.context_node_ngrams USING btree (context_id,  node_id);
        CREATE        INDEX ON public.context_node_ngrams USING btree (ngrams_id, node_id);
        CREATE        INDEX ON public.context_node_ngrams USING btree (ngrams_type);

        CREATE INDEX ON public.context_node_ngrams2 USING btree (context_id);
        CREATE INDEX ON public.context_node_ngrams2 USING btree (nodengrams_id);
        CREATE INDEX ON public.context_node_ngrams2 USING btree (context_id, nodengrams_id);

        DROP TABLE if EXISTS public.node_nodengrams_nodengrams;

        DROP TRIGGER if EXISTS trigger_count_delete2    ON nodes_nodes;
        DROP TRIGGER if EXISTS trigger_count_update_add ON nodes_nodes;
        DROP TRIGGER if EXISTS trigger_delete_count     ON nodes_nodes;
        DROP TRIGGER if EXISTS trigger_insert_count     ON nodes_nodes;


        -- Indexes needed to speed up the deletes
        -- Trigger for constraint node_ngrams_node_id_fkey
        CREATE INDEX IF NOT EXISTS node_ngrams_node_id_idx       ON public.node_ngrams USING btree (node_id);

        -- Trigger for constraint node_node_ngrams2_node_id_fkey
        CREATE INDEX IF NOT EXISTS node_node_ngrams2_node_id_idx ON public.node_node_ngrams2 USING btree (node_id);

        -- Trigger for constraint node_node_ngrams_node1_id_fkey
        CREATE INDEX IF NOT EXISTS node_node_ngrams_node1_id_idx  ON public.node_node_ngrams USING btree (node1_id);

        -- Trigger for constraint node_node_ngrams_node2_id_fkey
        CREATE INDEX IF NOT EXISTS node_node_ngrams_node2_id_idx  ON public.node_node_ngrams USING btree (node2_id);

        -- Trigger for constraint nodes_nodes_node1_id_fkey
        CREATE INDEX IF NOT EXISTS nodes_nodes_node1_id_idx ON public.nodes_nodes USING btree (node1_id);
        -- Trigger for constraint nodes_nodes_node2_id_fkey
        CREATE INDEX IF NOT EXISTS nodes_nodes_node2_id_idx ON public.nodes_nodes USING btree (node2_id);

        -- Trigger for constraint nodes_parent_id_fkey
        CREATE INDEX IF NOT EXISTS nodes_parent_id_idx ON public.nodes USING btree (parent_id);

        -- Trigger for constraint rights_node_id_fkey
        CREATE INDEX IF NOT EXISTS rights_node_id_idx ON public.rights USING btree (node_id);

        -- Trigger for constraint nodes_contexts_node_id_fkey
        CREATE INDEX IF NOT EXISTS nodes_contexts_node_id_idx ON public.nodes_contexts USING btree (node_id);

        -- Trigger for constraint context_node_ngrams_node_id_fkey
        CREATE INDEX IF NOT EXISTS context_node_node_id_idx ON public.context_node_ngrams USING btree (node_id);

  |]

-}
