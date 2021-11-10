
WITH listed AS
  ( select nn.ngrams_id AS id, count(*) AS c
    FROM node_node_ngrams nn 
    GROUP BY nn.ngrams_id
  )

--SELECT count(*) from listed l
-- WHERE
--l.c <= 1


DELETE FROM ngrams n 
  USING listed l
  WHERE
  n.id = l.id
  AND l.c <= 1
; 



