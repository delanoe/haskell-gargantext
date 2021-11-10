
WITH repeated AS
  ( select nn.node2_id AS id, count(*) AS c
    FROM nodes_nodes nn 
    GROUP BY nn.node2_id
  )

DELETE FROM nodes n 
USING repeated r
WHERE
n.id = r.id
AND r.c <= 1
; 

