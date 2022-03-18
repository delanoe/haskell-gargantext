
ALTER TABLE nodes_contexts DROP CONSTRAINT nodes_contexts_pkey;
ALTER TABLE nodes_contexts ADD COLUMN id SERIAL PRIMARY KEY ;

CREATE TABLE public.nodescontexts_nodescontexts (
    nodescontexts1 INTEGER NOT NULL REFERENCES public.nodes_contexts(id) ON DELETE CASCADE,
    nodescontexts2 INTEGER NOT NULL REFERENCES public.nodes_contexts(id) ON DELETE CASCADE,

    PRIMARY KEY (nodescontexts1, nodescontexts2)
);
ALTER TABLE public.nodescontexts_nodescontexts OWNER TO gargantua;

CREATE INDEX ON public.nodescontexts_nodescontexts USING btree (nodescontexts1, nodescontexts2)

