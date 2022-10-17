-- Adding index to improve ngrams_table query
create index node_node_ngrams_weight_idx on node_node_ngrams(weight);
