CREATE  INDEX ON public.node_node_ngrams USING btree (node1_id,  node2_id, ngrams_type);
