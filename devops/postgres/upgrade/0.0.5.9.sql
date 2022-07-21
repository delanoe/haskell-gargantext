create table public.node_stories (
  id SERIAL,
  node_id INTEGER NOT NULL,
  archive jsonb DEFAULT '{}'::jsonb NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE
);
ALTER TABLE public.node_stories OWNER TO gargantua;

CREATE UNIQUE INDEX ON public.node_stories USING btree (node_id);
