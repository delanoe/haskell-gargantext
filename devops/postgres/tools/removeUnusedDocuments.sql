
WITH repeated AS
  ( 
    select nn.context_id AS id, count(*) AS c
    FROM nodes_contexts nn 
    GROUP BY nn.context_id
  )

DELETE FROM contexts c
USING repeated r
WHERE
c.id = r.id
AND r.c = 1
AND c.typename = 4
; 

