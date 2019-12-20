CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
CREATE EXTENSION IF NOT EXISTS tsm_system_rows;

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

-- CREATE USER WITH ...
-- createdb "gargandb"

CREATE TABLE public.auth_user (
    id SERIAL,
    password     character varying(128) NOT NULL,
    last_login   timestamp with time zone,
    is_superuser boolean NOT NULL,
    username     character varying(150) NOT NULL,
    first_name   character varying(30) NOT NULL,
    last_name    character varying(30) NOT NULL,
    email        character varying(254) NOT NULL,
    is_staff     boolean NOT NULL,
    is_active    boolean NOT NULL,
    date_joined timestamp with time zone DEFAULT now() NOT NULL,
    PRIMARY KEY (id)
);

ALTER TABLE public.auth_user OWNER TO gargantua;


-- TODO add publication_date
-- TODO typename -> type_id
CREATE TABLE public.nodes (
    id        SERIAL,
    typename  integer NOT NULL,
    user_id   integer NOT NULL,
    parent_id integer REFERENCES public.nodes(id) ON DELETE CASCADE ,
    name      character varying(255) DEFAULT ''::character varying NOT NULL,
    date      timestamp with time zone DEFAULT now() NOT NULL,
    hyperdata jsonb DEFAULT '{}'::jsonb NOT NULL,
    search tsvector,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id)  REFERENCES public.auth_user(id) ON DELETE CASCADE
);
ALTER TABLE public.nodes OWNER TO gargantua;


CREATE TABLE public.ngrams (
    id SERIAL,
    terms character varying(255),
    n integer,
    PRIMARY KEY (id)
);
ALTER TABLE public.ngrams OWNER TO gargantua;

--------------------------------------------------------------
CREATE TABLE public.node_ngrams (
    id SERIAL,
    node_id integer NOT NULL,
    ngrams_id integer NOT NULL,
    list_type integer,
    ngrams_type integer, -- change to ngrams_field? (no for pedagogic reason)
    ngrams_field integer,
    ngrams_tag integer,
    ngrams_class integer,
    weight double precision,
    FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
    FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE,
    PRIMARY KEY (id)
);
ALTER TABLE public.node_ngrams OWNER TO gargantua;


CREATE TABLE public.node_node_ngrams_ngrams (
    node_id integer NOT NULL,
    node_ngrams1_id integer NOT NULL,
    node_ngrams2_id integer NOT NULL,
    FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
    FOREIGN KEY (node_ngrams1_id) REFERENCES public.node_ngrams(id) ON DELETE CASCADE,
    FOREIGN KEY (node_ngrams2_id) REFERENCES public.node_ngrams(id) ON DELETE CASCADE,
    PRIMARY KEY (node_id, node_ngrams1_id, node_ngrams2_id)
);
ALTER TABLE public.node_node_ngrams_ngrams OWNER TO gargantua;

--------------------------------------------------------------
--------------------------------------------------------------
--
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
    node1_id integer NOT NULL REFERENCES public.nodes(id) ON DELETE CASCADE,
    node2_id integer NOT NULL REFERENCES public.nodes(id) ON DELETE CASCADE,
    score real,
    category integer,
    PRIMARY KEY (node1_id,node2_id)
);
ALTER TABLE public.nodes_nodes OWNER TO gargantua;

---------------------------------------------------------------
-- TODO should reference "id" of nodes_nodes (instead of node1_id, node2_id)
CREATE TABLE public.node_node_ngrams (
node1_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
-- here id to node_ngrams
node2_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
ngrams_id  INTEGER NOT NULL REFERENCES public.ngrams (id) ON DELETE CASCADE,
ngrams_type INTEGER,
--ngrams_tag INTEGER,
--ngrams_class INTEGER,
weight double precision,
PRIMARY KEY (node1_id, node2_id, ngrams_id, ngrams_type)
);
ALTER TABLE public.node_node_ngrams OWNER TO gargantua;
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
-- INDEXES

CREATE INDEX        ON public.auth_user USING btree (username varchar_pattern_ops);
CREATE UNIQUE INDEX ON public.auth_user USING btree (username);

CREATE INDEX        ON public.rights USING btree (user_id,node_id);

CREATE INDEX        ON public.nodes USING gin (hyperdata);
CREATE INDEX        ON public.nodes USING btree (user_id, typename, parent_id);
CREATE INDEX        ON public.nodes USING btree (typename, id);
CREATE UNIQUE INDEX ON public.nodes USING btree (((hyperdata ->> 'uniqId'::text)));
CREATE UNIQUE INDEX ON public.nodes USING btree (((hyperdata ->> 'uniqIdBdd'::text)));
CREATE UNIQUE INDEX ON public.nodes USING btree (typename, parent_id, ((hyperdata ->> 'uniqId'::text)));

CREATE UNIQUE INDEX ON public.ngrams (terms); -- TEST GIN
CREATE        INDEX ON public.ngrams USING btree (id, terms);

CREATE INDEX        ON public.nodes_nodes  USING btree (node1_id, node2_id, category);
CREATE UNIQUE INDEX ON public.nodes_nodes  USING btree (node1_id, node2_id);

CREATE UNIQUE INDEX ON public.node_node_ngrams USING btree (node1_id, node2_id, ngrams_id, ngrams_type);
CREATE        INDEX ON public.node_node_ngrams USING btree (node1_id,  node2_id);
CREATE        INDEX ON public.node_node_ngrams USING btree (ngrams_id, node2_id);


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
create index node_by_pos on nodes using btree(node_pos(id,typename));

