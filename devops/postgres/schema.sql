CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

CREATE EXTENSION IF NOT EXISTS tsm_system_rows;
CREATE EXTENSION pgcrypto;

-----------------------------------------------------------------
CREATE TABLE public.auth_user (
    id SERIAL,
    password     CHARACTER varying(128) NOT NULL,
    last_login   TIMESTAMP with time zone,
    is_superuser BOOLEAN NOT NULL,
    username     CHARACTER varying(150) NOT NULL,
    first_name   CHARACTER varying(30) NOT NULL,
    last_name    CHARACTER varying(30) NOT NULL,
    email        CHARACTER varying(254) NOT NULL,
    is_staff     BOOLEAN NOT NULL,
    is_active    BOOLEAN NOT NULL,
    date_joined  TIMESTAMP with time zone DEFAULT now() NOT NULL,
    forgot_password_uuid TEXT,
    PRIMARY KEY (id)
);
ALTER TABLE public.auth_user OWNER TO gargantua;
-----------------------------------------------------------------

-- TODO add publication_date
-- TODO typename -> type_id
CREATE TABLE public.nodes (
    id        SERIAL,
    hash_id   CHARACTER varying(66) DEFAULT ''::character varying NOT NULL,
    typename  INTEGER NOT NULL,
    user_id   INTEGER NOT NULL,
    parent_id INTEGER REFERENCES public.nodes(id) ON DELETE CASCADE ,
    name      CHARACTER varying(255) DEFAULT ''::character varying NOT NULL,
    date      TIMESTAMP with time zone DEFAULT now() NOT NULL,
    hyperdata jsonb DEFAULT '{}'::jsonb NOT NULL,
    search tsvector,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id)  REFERENCES public.auth_user(id) ON DELETE CASCADE
);
ALTER TABLE public.nodes OWNER TO gargantua;
--------------------------------------------------------------

-- TODO add publication_date
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

--------------------------------------------------------------
-- | Ngrams
CREATE TABLE public.ngrams (
    id SERIAL,
    terms CHARACTER varying(255),
    n INTEGER,
    PRIMARY KEY (id)
);
ALTER TABLE public.ngrams OWNER TO gargantua;

-- | Ngrams PosTag
CREATE TABLE public.ngrams_postag (
    id        SERIAL                  ,
    lang_id   INTEGER                 ,
    algo_id   INTEGER                 ,
    postag    CHARACTER varying(5)    ,
    ngrams_id INTEGER NOT NULL        ,
    lemm_id   INTEGER NOT NULL        ,
    score     INTEGER DEFAULT 1 ::integer NOT NULL                        ,
    FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE,
    FOREIGN KEY (lemm_id)   REFERENCES public.ngrams(id) ON DELETE CASCADE
);
ALTER TABLE public.ngrams_postag OWNER TO gargantua;

--------------------------------------------------------------
-- Node here should have type NodeList
CREATE TABLE public.node_ngrams (
    id SERIAL                          ,
    node_id        INTEGER NOT NULL    ,
    node_subtype   INTEGER             ,
    ngrams_id      INTEGER NOT NULL    ,
    ngrams_type    INTEGER             , -- change to ngrams_field? (no for pedagogic reason)
    ngrams_field   INTEGER             ,
    ngrams_tag     INTEGER             ,
    ngrams_class   INTEGER             ,
    weight double precision            ,
    PRIMARY KEY (id)                   ,
    FOREIGN KEY (node_id)   REFERENCES public.nodes(id)  ON DELETE CASCADE ,
    FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE
);
ALTER TABLE public.node_ngrams OWNER TO gargantua;

--CREATE TABLE public.context_nodengrams_nodengrams (
--    context_id         INTEGER NOT NULL   ,
--    node_ngrams1_id INTEGER NOT NULL   ,
--    node_ngrams2_id INTEGER NOT NULL   ,
--    weight double   precision          ,
--    FOREIGN KEY (node_id) REFERENCES public.contexts(id) ON DELETE CASCADE              ,
--    FOREIGN KEY (node_ngrams1_id) REFERENCES public.node_ngrams(id) ON DELETE CASCADE,
--    FOREIGN KEY (node_ngrams2_id) REFERENCES public.node_ngrams(id) ON DELETE CASCADE,
--    PRIMARY KEY (node_id, node_ngrams1_id, node_ngrams2_id)
--);
--ALTER TABLE public.context_nodengrams_nodengrams OWNER TO gargantua;

--------------------------------------------------------------
--------------------------------------------------------------
--
--CREATE TABLE public.nodes_ngrams_ngrams (
--    node_id   integer NOT NULL REFERENCES public.nodes(id)  ON DELETE CASCADE,
--    ngram1_id integer NOT NULL REFERENCES public.ngrams(id) ON DELETE CASCADE,
--    ngram2_id integer NOT NULL REFERENCES public.ngrams(id) ON DELETE CASCADE,
--    weight double precision,
--    PRIMARY KEY (node_id,ngram1_id,ngram2_id)
--);
--
--ALTER TABLE public.nodes_ngrams_ngrams OWNER TO gargantua;
---------------------------------------------------------------
-- TODO nodes_nodes(node1_id int, node2_id int, edge_type int , weight real)
CREATE TABLE public.nodes_nodes (
    node1_id INTEGER NOT NULL REFERENCES public.nodes(id) ON DELETE CASCADE,
    node2_id INTEGER NOT NULL REFERENCES public.nodes(id) ON DELETE CASCADE,
    score    REAL    ,
    category INTEGER ,
    PRIMARY KEY (node1_id, node2_id)
);
ALTER TABLE public.nodes_nodes OWNER TO gargantua;



-- To attach contexts to a Corpus
CREATE TABLE public.nodes_contexts (
    id SERIAL                          ,
    node_id    INTEGER NOT NULL REFERENCES public.nodes(id)    ON DELETE CASCADE,
    context_id INTEGER NOT NULL REFERENCES public.contexts(id) ON DELETE CASCADE,
    score    REAL    ,
    category INTEGER ,
    PRIMARY KEY (id)
);
ALTER TABLE public.nodes_contexts OWNER TO gargantua;

CREATE TABLE public.nodescontexts_nodescontexts (
    nodescontexts1 INTEGER NOT NULL REFERENCES public.nodes_contexts(id) ON DELETE CASCADE,
    nodescontexts2 INTEGER NOT NULL REFERENCES public.nodes_contexts(id) ON DELETE CASCADE,

    PRIMARY KEY (nodescontexts1, nodescontexts2)
);
ALTER TABLE public.nodescontexts_nodescontexts OWNER TO gargantua;


---------------------------------------------------------------
CREATE TABLE public.context_node_ngrams (
    context_id    INTEGER NOT NULL REFERENCES public.contexts (id) ON DELETE CASCADE,
    node_id       INTEGER NOT NULL REFERENCES public.nodes    (id) ON DELETE CASCADE,
    ngrams_id     INTEGER NOT NULL REFERENCES public.ngrams   (id) ON DELETE CASCADE,
    ngrams_type   INTEGER  ,
    weight double precision,
    doc_count     INTEGER,
    PRIMARY KEY (context_id, node_id, ngrams_id, ngrams_type)
  );
ALTER TABLE public.context_node_ngrams OWNER TO gargantua;

CREATE TABLE public.context_node_ngrams2 (
    context_id      INTEGER NOT NULL REFERENCES public.contexts     (id) ON DELETE CASCADE,
    nodengrams_id   INTEGER NOT NULL REFERENCES public.node_ngrams  (id) ON DELETE CASCADE,
    weight double   precision,
    PRIMARY KEY (context_id, nodengrams_id)
);
ALTER TABLE public.context_node_ngrams2 OWNER TO gargantua;


--------------------------------------------------------------------
CREATE TABLE public.node_node_ngrams (
node1_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
node2_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
ngrams_id  INTEGER NOT NULL REFERENCES public.ngrams (id) ON DELETE CASCADE,
ngrams_type INTEGER,
weight double precision,
PRIMARY KEY (node1_id, node2_id, ngrams_id, ngrams_type)
);
ALTER TABLE public.node_node_ngrams OWNER TO gargantua;

CREATE TABLE public.node_node_ngrams2 (
node_id         INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
nodengrams_id   INTEGER NOT NULL REFERENCES public.node_ngrams  (id) ON DELETE CASCADE,
weight double precision,
PRIMARY KEY (node_id, nodengrams_id)
);
ALTER TABLE public.node_node_ngrams2 OWNER TO gargantua;


--------------------------------------------------------------

--CREATE TABLE public.nodes_ngrams_repo (
--    version integer NOT NULL,
--    patches jsonb DEFAULT '{}'::jsonb NOT NULL,
--    PRIMARY KEY (version)
--);
--ALTER TABLE public.nodes_ngrams_repo OWNER TO gargantua;

---------------------------------------------------------

-- If needed for rights management at row level
-- CREATE EXTENSION IF NOT EXISTS acl WITH SCHEMA public;
CREATE TABLE public.rights (
  user_id INTEGER NOT NULL REFERENCES public.auth_user(id) ON DELETE CASCADE,
  node_id INTEGER NOT NULL REFERENCES public.nodes(id)     ON DELETE CASCADE,
  rights  INTEGER NOT NULL,
  PRIMARY KEY (user_id, node_id)
);
ALTER TABLE public.rights OWNER TO gargantua;

------------------------------------------------------------
-- Node Story

CREATE TABLE public.node_stories (
  id SERIAL,
  node_id INTEGER NOT NULL,
  version INTEGER NOT NULL,
  ngrams_type_id INTEGER NOT NULL,
  ngrams_id INTEGER NOT NULL,
  --children TEXT[],
  ngrams_repo_element jsonb DEFAULT '{}'::jsonb NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
  FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE
);
ALTER TABLE public.node_stories OWNER TO gargantua;

CREATE UNIQUE INDEX ON public.node_stories USING btree (node_id, ngrams_type_id, ngrams_id);


create table public.node_story_archive_history (
  id SERIAL,
  node_id INTEGER NOT NULL,
  ngrams_type_id INTEGER NOT NULL,
  ngrams_id INTEGER NOT NULL,
  patch jsonb DEFAULT '{}'::jsonb NOT NULL,
  version INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (id),
  FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
  FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE
);
ALTER TABLE public.node_story_archive_history OWNER TO gargantua;


------------------------------------------------------------
-- INDEXES
CREATE INDEX        ON public.auth_user USING btree (username varchar_pattern_ops);
CREATE UNIQUE INDEX ON public.auth_user USING btree (username);

CREATE INDEX        ON public.rights USING btree (user_id,node_id);

CREATE INDEX        ON public.nodes USING gin (hyperdata);
CREATE INDEX        ON public.nodes USING btree (user_id, typename, parent_id);
CREATE INDEX        ON public.nodes USING btree (id, typename, date ASC);
CREATE INDEX        ON public.nodes USING btree (id, typename, date DESC);
CREATE INDEX        ON public.nodes USING btree (typename, id);
CREATE UNIQUE INDEX ON public.nodes USING btree (hash_id);

CREATE INDEX        ON public.contexts USING gin (hyperdata);
CREATE INDEX        ON public.contexts USING btree (user_id, typename, parent_id);
CREATE INDEX        ON public.contexts USING btree (id, typename, date ASC);
CREATE INDEX        ON public.contexts USING btree (id, typename, date DESC);
CREATE INDEX        ON public.contexts USING btree (typename, id);
CREATE UNIQUE INDEX ON public.contexts USING btree (hash_id);

CREATE INDEX ON public.nodescontexts_nodescontexts USING btree (nodescontexts1, nodescontexts2);
-- CREATE UNIQUE INDEX ON public.nodes USING btree (((hyperdata ->> 'uniqId'::text)));
-- CREATE UNIQUE INDEX ON public.nodes USING btree (((hyperdata ->> 'uniqIdBdd'::text)));
-- CREATE UNIQUE INDEX ON public.nodes USING btree (typename, parent_id, ((hyperdata ->> 'uniqId'::text)));

CREATE UNIQUE INDEX ON public.ngrams (terms); -- TEST GIN
CREATE        INDEX ON public.ngrams USING btree (id, terms);
CREATE UNIQUE INDEX ON public.ngrams_postag (lang_id,algo_id,postag,ngrams_id,lemm_id);

-- To save the Node Ngrams Repo
CREATE        INDEX ON public.node_ngrams USING btree (node_id,node_subtype);
CREATE UNIQUE INDEX ON public.node_ngrams USING btree (node_id,node_subtype, ngrams_id);



-- To make the links between Nodes in Tree/Forest
CREATE UNIQUE INDEX ON public.nodes_nodes  USING btree (node1_id, node2_id);
CREATE INDEX        ON public.nodes_nodes  USING btree (node1_id, node2_id, category);


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


CREATE UNIQUE INDEX ON public.node_node_ngrams USING btree (node1_id, node2_id, ngrams_id, ngrams_type);
CREATE        INDEX ON public.node_node_ngrams USING btree (node1_id,  node2_id);
CREATE        INDEX ON public.node_node_ngrams USING btree (ngrams_id, node2_id);
CREATE        INDEX ON public.node_node_ngrams USING btree (ngrams_type);
CREATE INDEX ON public.node_node_ngrams2 USING btree (node_id);
CREATE INDEX ON public.node_node_ngrams2 USING btree (nodengrams_id);
CREATE INDEX ON public.node_node_ngrams2 USING btree (node_id, nodengrams_id);

-- CREATE INDEX ON public.context_nodengrams_nodengrams USING btree (context_id, node_ngrams1_id, node_ngrams2_id);
-- CREATE INDEX ON public.context_nodengrams_nodengrams USING btree (node_ngrams1_id);
-- CREATE INDEX ON public.context_nodengrams_nodengrams USING btree (node_ngrams2_id);


------------------------------------------------------------------------
-- Ngrams Full DB Extraction Optim
-- TODO remove hard parameter and move elsewhere
CREATE OR REPLACE function node_pos(int, int) returns bigint
   AS 'SELECT count(id) from nodes
      WHERE  id < $1
      AND typename = $2
      '
   LANGUAGE SQL immutable;

--drop index node_by_pos;
--create index node_by_pos on nodes using btree(node_pos(id,typename));
