
ALTER TABLE nodes
DROP COLUMN IF EXISTS search_title,
DROP COLUMN IF EXISTS tsvector;

ALTER TABLE nodes ADD COLUMN search_title tsvector;
UPDATE nodes SET search_title = to_tsvector('english', coalesce("hyperdata"->>'title', '') || ' ' || coalesce("hyperdata"->>'abstract', ''));
CREATE INDEX search_title_idx ON nodes USING GIN (search_title);




