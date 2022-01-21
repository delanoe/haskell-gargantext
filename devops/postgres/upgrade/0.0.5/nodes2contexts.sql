-- to delete
-- DELETE FROM contexts;

-- WITH docs (id,hash_id,typename,user_id,parent_id,name,date,hyperdata, search)
WITH docs AS (SELECT * from nodes  WHERE nodes.typename IN (4,41)),

inserted (id, hash_id) AS (
  INSERT INTO contexts (hash_id,typename,user_id,parent_id,name,date,hyperdata, search)
    SELECT d.hash_id,d.typename,d.user_id,NULL,d.name,d.date,d.hyperdata,search FROM docs AS d
    RETURNING contexts.id, contexts.hash_id
 ),

indexed (node_id, context_id) AS (
  SELECT docs.id, inserted.id from inserted
  JOIN docs on docs.hash_id = inserted.hash_id
),

-- nodes_nodes -> nodes_contexts
nodes_contexts_query AS (
INSERT INTO nodes_contexts (node_id, context_id,score, category)
SELECT nn.node1_id,i.context_id,nn.score,nn.category FROM nodes_nodes nn
JOIN indexed i ON i.node_id = nn.node2_id
),

-- nodes_nodes_ngrams -> contexts_nodes_ngrams
contexts_nodes_ngrams_query AS (
INSERT INTO context_node_ngrams
SELECT i.context_id, nnn.node1_id, nnn.ngrams_id, nnn.ngrams_type, nnn.weight FROM node_node_ngrams nnn
JOIN indexed i ON i.node_id = nnn.node2_id
),

---- nodes_nodes_ngrams2 -> contexts_nodes_ngrams2
context_node_ngrams2_query AS (
INSERT INTO context_node_ngrams2
SELECT i.context_id, nnn2.nodengrams_id, nnn2.weight FROM node_node_ngrams2 nnn2
JOIN indexed i ON i.node_id = nnn2.node_id
)

-- WITH CASCADE it should update others tables
DELETE FROM nodes n
USING indexed i WHERE i.node_id = n.id
;

UPDATE contexts SET parent_id = id;


