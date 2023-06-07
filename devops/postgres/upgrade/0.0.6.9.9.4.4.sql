-- create materialized view if  not exists context_node_ngrams_view as
-- select context_node_ngrams.context_id, ngrams_id, nodes_contexts.node_id
-- from nodes_contexts
-- join context_node_ngrams
-- on context_node_ngrams.context_id = nodes_contexts.context_id;

-- create index if not exists context_node_ngrams_view_context_id_idx on context_node_ngrams_view(context_id);
-- create index if not exists context_node_ngrams_view_ngrams_id_idx on context_node_ngrams_view(ngrams_id);
-- create index if not exists context_node_ngrams_view_node_id_idx on context_node_ngrams_view(node_id);

create index if not exists context_node_ngrams_context_id_ngrams_id_idx on context_node_ngrams(context_id, ngrams_id);
create index if not exists node_stories_ngrams_id_idx on node_stories(ngrams_id);
