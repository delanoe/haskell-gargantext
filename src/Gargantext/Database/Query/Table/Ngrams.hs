{-|
Module      : Gargantext.Database.Query.Table.Ngrams
Description : Deal with in Gargantext Database.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Database.Query.Table.Ngrams
  ( module Gargantext.Database.Schema.Ngrams
  , queryNgramsTable
  , selectNgramsByDoc
  , insertNgrams
  )
    where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Text (Text)
import Data.Map (Map, fromList)
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Prelude (runOpaQuery, Cmd)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Prelude (runPGSQuery, formatPGSQuery)
import Gargantext.Database.Query.Table.NodeNodeNgrams
import Gargantext.Prelude
import Gargantext.Database.Schema.Prelude
import Data.ByteString.Internal (ByteString)
import qualified Database.PostgreSQL.Simple as PGS

queryNgramsTable :: Query NgramsRead
queryNgramsTable = queryTable ngramsTable

selectNgramsByDoc :: [ListId] -> DocId -> NgramsType -> Cmd err [Text]
selectNgramsByDoc lIds dId nt = runOpaQuery (query lIds dId nt)
  where

    join :: Query (NgramsRead, NodeNodeNgramsReadNull)
    join = leftJoin queryNgramsTable queryNodeNodeNgramsTable on1
      where
        on1 (ng,nnng) = ng^.ngrams_id .== nnng^.nnng_ngrams_id

    query cIds' dId' nt' = proc () -> do
      (ng,nnng) <- join -< ()
      restrict -< foldl (\b cId -> ((toNullable $ pgNodeId cId) .== nnng^.nnng_node1_id) .|| b) (pgBool True) cIds'
      restrict -< (toNullable $ pgNodeId dId')    .== nnng^.nnng_node2_id
      restrict -< (toNullable $ pgNgramsType nt') .== nnng^.nnng_ngramsType
      returnA  -< ng^.ngrams_terms


_postNgrams :: CorpusId -> DocId -> [Text] -> Cmd err Int
_postNgrams = undefined

_dbGetNgramsDb :: Cmd err [NgramsDb]
_dbGetNgramsDb = runOpaQuery queryNgramsTable


-- TODO-ACCESS: access must not be checked here but when insertNgrams is called.
insertNgrams :: [Ngrams] -> Cmd err (Map NgramsTerms NgramsId)
insertNgrams ns = fromList <$> map (\(NgramIds i t) -> (t, i)) <$> (insertNgrams' ns)

-- TODO-ACCESS: access must not be checked here but when insertNgrams' is called.
insertNgrams' :: [Ngrams] -> Cmd err [NgramIds]
insertNgrams' ns = runPGSQuery queryInsertNgrams (PGS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

_insertNgrams_Debug :: [(NgramsTerms, Size)] -> Cmd err ByteString
_insertNgrams_Debug ns = formatPGSQuery queryInsertNgrams (PGS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

----------------------
queryInsertNgrams :: PGS.Query
queryInsertNgrams = [sql|
    WITH input_rows(terms,n) AS (?)
    , ins AS (
       INSERT INTO ngrams (terms,n)
       SELECT * FROM input_rows
       ON CONFLICT (terms) DO NOTHING -- unique index created here
       RETURNING id,terms
       )

    SELECT id, terms
    FROM   ins
    UNION  ALL
    SELECT c.id, terms
    FROM   input_rows
    JOIN   ngrams c USING (terms);     -- columns of unique index
           |]




