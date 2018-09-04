{-|
Module      : Gargantext.Database.TextSearch
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

module Gargantext.Database.Cooc where

import Control.Monad ((>>=))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Gargantext.Prelude
import Gargantext (connectGargandb)

type CorpusId    = Int
type MainListId  = Int
type GroupListId = Int

coocTest :: IO [(Int, Int, Int)]
coocTest = connectGargandb "gargantext.ini"
  >>= \conn -> dBcooc conn 421968 446602 446599 

dBcooc :: Connection -> CorpusId -> MainListId -> GroupListId -> IO [(Int, Int, Int)]
dBcooc conn corpus mainList groupList = query conn [sql|
  set work_mem='1GB';

  --EXPLAIN ANALYZE
      WITH COOC as (
      SELECT
      COALESCE(grA.ngram1_id, wlA.ngram_id) as ngA,
      COALESCE(grB.ngram1_id, wlB.ngram_id) as ngB,
      COUNT(*) AS score
      FROM
      nodes AS n
      --      /     --     X   Y
      -- SQL graph for getting the cooccurrences

          -- STEP 1: X axis of the matrix
          INNER JOIN nodes_ngrams
                  AS ngA  ON ngA.node_id  = n.id
          -- \--> get the occurrences node/ngram of the corpus

              INNER JOIN nodes_ngrams
                      AS wlA  ON ngA.ngram_id = wlA.ngram_id
                             AND wlA.node_id  = ?
              -- \--> filter with white/main list (typename 7)

          LEFT JOIN  nodes_ngrams_ngrams
                 AS grA  ON wlA.ngram_id = grA.ngram1_id
                        AND grA.node_id  = ?
          -- \--> adding (joining) ngrams that are grouped (typename 6)
          LEFT JOIN  nodes_ngrams
                 AS wlAA ON grA.ngram2_id = wlAA.ngram_id
                        AND wlAA.node_id  = wlA.node_id
          -- \--> adding (joining) ngrams that are not grouped
          --LEFT JOIN  ngrams        AS wlAA ON grA.ngram2_id = wlAA.id
          -- \--> for joining all synonyms even if they are not in the main list (white list)


          -- STEP 2: Y axi of the matrix
          INNER JOIN nodes_ngrams
                  AS ngB  ON ngB.node_id  = n.id
          -- \--> get the occurrences node/ngram of the corpus

              INNER JOIN nodes_ngrams
                      AS wlB  ON ngB.ngram_id = wlB.ngram_id
                             AND wlB.node_id  = ?
              -- \--> filter with white/main list

          LEFT JOIN  nodes_ngrams_ngrams
                 AS grB  ON wlB.ngram_id = grB.ngram1_id
                        AND grB.node_id  = ?
          -- \--> adding (joining) ngrams that are grouped
          LEFT JOIN  nodes_ngrams
                 AS wlBB ON grB.ngram2_id = wlBB.ngram_id
                        AND wlBB.node_id   = wlB.node_id
          -- \--> adding (joining) ngrams that are not grouped

          -- LEFT JOIN  ngrams        AS wlBB ON grB.ngram2_id = wlBB.id
          -- \--> for joining all synonyms even if they are not in the main list (white list)

          WHERE
              n.typename  = 4
          AND n.parent_id = ?
          GROUP BY 1,2
          --    ==
          -- GROUP BY ngA, ngB
          )

      SELECT ngA, ngB, score
          FROM COOC    --> from the query above

   WHERE score >= 3
  AND 
  ngA <= ngB
  |] (mainList, groupList, mainList, groupList, corpus)


