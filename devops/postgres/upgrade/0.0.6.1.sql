-- we will migrate data here
-- rename old table and create a new one

ALTER TABLE public.node_stories RENAME TO node_stories_old;

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

-- Authors (ngrams_type_id = 1), see G.D.S.Ngrams.hs -> ngramsTypeId
INSERT INTO public.node_stories
  (node_id, version, ngrams_type_id, ngrams_id, ngrams_repo_element)
  SELECT node_id, (archive->'version')::int, 1, ngrams.id, j.value
    FROM node_stories_old
    CROSS JOIN jsonb_each(archive->'state'->'Authors') AS j
    JOIN ngrams ON terms = j.key;
-- we will leave children for later, small steps
-- INSERT INTO public.node_stories
--   (node_id, version, ngrams_type_id, ngrams_id, children, ngrams_repo_element)
--   SELECT node_id, (archive->'version')::int, 1, ngrams.id, c.children, (j.value - 'children')
--     FROM node_stories_old
--     CROSS JOIN jsonb_each(archive->'state'->'Authors') AS j
--     CROSS JOIN LATERAL (SELECT array_agg(d.elem) AS children FROM jsonb_array_elements_text(j.value->'children') AS d(elem)) AS c
--     JOIN ngrams ON terms = j.key;

-- Institutes (ngrams_type_id = 2)
INSERT INTO public.node_stories
  (node_id, version, ngrams_type_id, ngrams_id, ngrams_repo_element)
  SELECT node_id, (archive->'version')::int, 2, ngrams.id, j.value
    FROM node_stories_old
    CROSS JOIN jsonb_each(archive->'state'->'Institutes') AS j
  JOIN ngrams ON terms = j.key;
-- Sources (ngrams_type_id = 3)
INSERT INTO public.node_stories
  (node_id, version, ngrams_type_id, ngrams_id, ngrams_repo_element)
  SELECT node_id, (archive->'version')::int, 3, ngrams.id, j.value
    FROM node_stories_old
    CROSS JOIN jsonb_each(archive->'state'->'Sources') AS j
    JOIN ngrams ON terms = j.key;
-- NgramsTerms (ngrams_type_id = 4)
INSERT INTO public.node_stories
  (node_id, version, ngrams_type_id, ngrams_id, ngrams_repo_element)
  SELECT node_id, (archive->'version')::int, 4, ngrams.id, j.value
    FROM node_stories_old
    CROSS JOIN jsonb_each(archive->'state'->'NgramsTerms') AS j
    JOIN ngrams ON terms = j.key;
