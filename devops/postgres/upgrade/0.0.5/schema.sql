
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


DELETE TABLE public.node_nodengrams_nodengrams
