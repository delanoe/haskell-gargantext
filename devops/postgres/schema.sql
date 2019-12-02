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
-- TODO: delete delete this table
--CREATE TABLE public.nodes_ngrams (
--    id SERIAL,
--    node_id integer NOT NULL,
--    ngrams_id integer NOT NULL,
--    parent_id integer REFERENCES public.nodes_ngrams(id) ON DELETE SET NULL,
--    ngrams_type integer,
--    list_type integer,
--    weight double precision,
--    FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
--    FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE,
--    PRIMARY KEY (id)
--);
--ALTER TABLE public.nodes_ngrams OWNER TO gargantua;
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
id SERIAL,
node1_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
node2_id   INTEGER NOT NULL REFERENCES public.nodes  (id) ON DELETE CASCADE,
ngrams_id  INTEGER NOT NULL REFERENCES public.ngrams (id) ON DELETE CASCADE,
ngrams_type INTEGER,
weight double precision,
PRIMARY KEY (id)
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

-- TRIGGERS
-- TODO user haskell-postgresql-simple to create this function
-- with rights typename
CREATE OR REPLACE FUNCTION public.search_update()
RETURNS trigger AS $$
begin
  IF new.typename = 4 AND new.hyperdata @> '{"language_iso2":"EN"}' THEN
    new.search := to_tsvector( 'english' , (new.hyperdata ->> 'title') || ' ' || (new.hyperdata ->> 'abstract'));
  
  ELSIF new.typename = 4 AND new.hyperdata @> '{"language_iso2":"FR"}' THEN
    new.search := to_tsvector( 'french' , (new.hyperdata ->> 'title') || ' ' || (new.hyperdata ->> 'abstract'));
  
  ELSIF new.typename = 41 THEN
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

CREATE TRIGGER search_update_trigger BEFORE INSERT OR UPDATE ON nodes FOR EACH ROW EXECUTE PROCEDURE search_update();

-- Ngrams Full DB Extraction Optim
-- TODO remove hard parameter
CREATE OR REPLACE function node_pos(int, int) returns bigint
   AS 'SELECT count(id) from nodes
      WHERE  id < $1
      AND typename = $2
      '
   LANGUAGE SQL immutable;

--drop index node_by_pos;
create index node_by_pos on nodes using btree(node_pos(id,typename));

-- Initialize index with already existing data
UPDATE nodes SET hyperdata = hyperdata;






