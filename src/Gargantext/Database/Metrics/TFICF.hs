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
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Metrics.TFICF where

import Data.Text (Text)
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

data TficfTerms = TficfTerms
  { tt_terms  :: !Text
  , tt_global :: !Double
  , tt_corpus :: !Double
  } deriving (Show)

data TficfData = TficfData
  { td_global :: !Double
  , td_corpus :: !Double
  , td_terms  :: ![TficfTerms]
  } deriving (Show)

getTficf :: UsernameMaster -> CorpusId -> NgramsType
          -> Cmd err TficfData
getTficf u cId ngType = do
  g   <- countDocsInDatabase u
  c   <- countDocsInCorpus   cId
  ngs <- getOccByNgrams u cId ngType

  pure $ TficfData (fromIntegral g) (fromIntegral c) ngs


-- | TODO add filters with LANG and Database type
countDocsInDatabase :: UsernameMaster -> Cmd err Int
countDocsInDatabase u = maybe 0 identity <$> headMay
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

countDocsInCorpus :: CorpusId -> Cmd err Int
countDocsInCorpus cId = maybe 0 identity <$> headMay
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



getOccByNgrams :: UsernameMaster -> CorpusId -> NgramsType
               -> Cmd err [TficfTerms]
getOccByNgrams u cId ngType = map (\(t,g,c) -> TficfTerms t g c)
                           <$> runPGSQuery queryTficf p
  where
    p = (u, nodeTypeId NodeDocument, ngramsTypeId ngType, cId)


queryTficf :: DPS.Query
queryTficf = [sql|
WITH input(masterUsername,typenameDoc,ngramsTypeId,corpusId) 
  AS ((VALUES(?::"text", ?::"int4",?::"int4",?::"int4"))),

ngrams_master AS (
SELECT ng.id, ng.terms, SUM(nng2.weight) AS weight FROM nodes_ngrams nng
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

JOIN nodes_ngrams nng2 ON nng2.ngrams_id = nng.ngrams_id
JOIN nodes_nodes  nn   ON nn.node2_id    = nng2.node_id

JOIN ngrams       ng   ON ng.id          = nng2.ngrams_id
JOIN input             ON nn.node1_id    = input.corpusId

WHERE
     nng.ngrams_type = input.ngramsTypeId
-- AND n.hyperdata -> 'lang' = 'en'
GROUP BY ng.id,ng.terms
)


SELECT nu.terms,SUM(nm.weight) wm,SUM(nu.weight) wu
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


