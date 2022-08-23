create table public.node_stories (
  id SERIAL,
  node_id INTEGER NOT NULL,
  archive jsonb DEFAULT '{}'::jsonb NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE
);
ALTER TABLE public.node_stories OWNER TO gargantua;

CREATE UNIQUE INDEX ON public.node_stories USING btree (node_id);



create table public.node_story_archive_history (
  id SERIAL,
  node_id INTEGER NOT NULL,
  ngrams_type_id INTEGER NOT NULL,
  ngrams_id INTEGER NOT NULL,
  patch jsonb DEFAULT '{}'::jsonb NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE,
  FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE
);
ALTER TABLE public.node_story_archive_history OWNER TO gargantua;


-- INSERT INTO node_story_archive_history (node_id, ngrams_type_id, patch) SELECT t.node_id, t.ngrams_type_id, t.patch FROM
-- (
-- WITH q AS (SELECT node_id, history.*, row_number() over (ORDER BY node_id) AS sid
-- FROM node_stories,
-- jsonb_to_recordset(archive->'history') AS history("Authors" jsonb, "Institutes" jsonb, "NgramsTerms" jsonb, "Sources" jsonb))

-- (SELECT node_id, sid, 1 AS ngrams_type_id, "Authors" AS patch FROM q WHERE "Authors" IS NOT NULL)
-- UNION (SELECT node_id, sid, 2 AS ngrams_type_id, "Institutes" AS patch FROM q WHERE "Institutes" IS NOT NULL)
-- UNION (SELECT node_id, sid, 4 AS ngrams_type_id, "NgramsTerms" AS patch FROM q WHERE "NgramsTerms" IS NOT NULL)
-- UNION (SELECT node_id, sid, 3 AS ngrams_type_id, "Sources" AS patch FROM q WHERE "Sources" IS NOT NULL)
-- ORDER BY node_id, ngrams_type_id, sid
-- ) AS t;

