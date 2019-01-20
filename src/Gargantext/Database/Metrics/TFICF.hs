{-|
Module      : Gargantext.Database.Metrics.TFICF
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TFICF, generalization of TFIDF

-}

{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}

module Gargantext.Database.Metrics.TFICF where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple as DPS

import Safe (headMay)
import Gargantext.Text.Metrics.TFICF -- (tficf)
import Gargantext.Prelude
import Gargantext.Core.Types.Individu (UsernameMaster)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Database.Types.Node (ListId, CorpusId, NodeType(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Ngrams (NgramsId, NgramsTerms, NgramsType, ngramsTypeId)

type OccGlobal = Double
type OccCorpus = Double


getTficf :: UsernameMaster -> CorpusId -> ListId -> NgramsType
          -> Cmd err [Tficf]
getTficf u cId lId ngType = do
  g   <- getTficfGlobal u
  c   <- getTficfCorpus cId
  ngs <- getTficfNgrams u cId lId ngType

  pure $ map (\(nId, nTerms, wm, wn)
        -> Tficf nId nTerms
                 (tficf (TficfCorpus   wn (fromIntegral c))
                        (TficfLanguage wm (fromIntegral g))
                  )
              ) ngs

getTficfGlobal :: UsernameMaster -> Cmd err Int
getTficfGlobal u = maybe 0 identity <$> headMay
                <$> map (\(DPS.Only n) -> n )
                <$> runPGSQuery q p
  where
    p = (u, nodeTypeId NodeDocument)
    q = [sql| SELECT count(*) from nodes n
              JOIN auth_user a ON a.id = n.user_id
              WHERE
              a.username = ?
              AND n.typename = ?
  |]

getTficfCorpus :: CorpusId -> Cmd err Int
getTficfCorpus cId = maybe 0 identity <$> headMay
                  <$> map (\(DPS.Only n) -> n )
                  <$> runPGSQuery q p
  where
    p = (cId, nodeTypeId NodeDocument)
    q = [sql| WITH input(corpusId, typename) AS ((VALUES(?::"int4",?::"int4")))
              SELECT count(*) from nodes_nodes AS nn
              JOIN nodes AS n ON n.id = nn.node2_id
              JOIN input ON nn.node1_id = input.corpusId
              WHERE n.typename = input.typename;
  |]



getTficfNgrams :: UsernameMaster -> CorpusId -> ListId -> NgramsType
             -> Cmd err [(NgramsId, NgramsTerms, OccGlobal, OccCorpus)]
getTficfNgrams u cId lId ngType = runPGSQuery queryTficf p
  where
    p = (u, nodeTypeId NodeList, nodeTypeId NodeDocument, ngramsTypeId ngType, cId, lId)


queryTficf :: DPS.Query
queryTficf = [sql|
-- TODO add CTE for groups
WITH input(masterUsername,typenameList,typenameDoc,ngramsTypeId,corpusId,listId) 
  AS ((VALUES(?::"text", ? :: "int4", ?::"int4", ?::"int4",?::"int4",?::"int4"))),
  -- AS ((VALUES('gargantua'::"text", 5 :: "int4", 4::"int4", 4::"int4",1018::"int4",1019::"int4"))),

list_master AS (
SELECT n.id,n.name,n.user_id from nodes n
JOIN input       ON n.typename = input.typenameList
JOIN auth_user a ON a.id       = n.user_id
WHERE
a.username = input.masterUsername
),

ngrams_master AS (
SELECT ng.id, ng.terms, SUM(nng2.weight) AS weight FROM nodes_ngrams nng
JOIN list_master       ON list_master.id       = nng.node_id
JOIN nodes_ngrams nng2 ON nng2.ngrams_id = nng.ngrams_id
JOIN nodes        n   ON n.id          = nng2.node_id
JOIN input            ON input.typenameDoc = n.typename
JOIN ngrams       ng  ON ng.id         = nng2.ngrams_id
WHERE
 nng.ngrams_type = input.ngramsTypeId
-- AND n.hyperdata -> 'lang' = 'en'
GROUP BY ng.id,ng.terms
),

ngrams_user AS (
SELECT ng.id, ng.terms, SUM(nng2.weight) AS weight
FROM nodes_ngrams nng
JOIN list_master       ON list_master.id        = nng.node_id

JOIN nodes_ngrams nng2 ON nng2.ngrams_id = nng.ngrams_id
JOIN nodes_nodes  nn   ON nn.node2_id    = nng2.node_id

JOIN ngrams       ng   ON ng.id          = nng2.ngrams_id
JOIN input             ON nn.node1_id    = input.corpusId

WHERE
     nng.ngrams_type = input.ngramsTypeId
-- AND n.hyperdata -> 'lang' = 'en'
GROUP BY ng.id,ng.terms
)


SELECT nu.id,nu.terms,SUM(nm.weight) wm,SUM(nu.weight) wu
FROM ngrams_user nu
JOIN ngrams_master nm ON nm.id = nu.id
WHERE
nm.weight > 1
AND
nu.weight > 1
GROUP BY nu.id,nu.terms
--ORDER BY wm DESC
--LIMIT 1000

  |]



