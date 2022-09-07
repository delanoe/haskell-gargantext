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
  , selectNgramsId
  )
    where

import Control.Lens ((^.))
import Data.ByteString.Internal (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Database.Prelude (runOpaQuery, Cmd, formatPGSQuery, runPGSQuery)
import Gargantext.Database.Query.Join (leftJoin3)
import Gargantext.Database.Query.Table.ContextNodeNgrams2
import Gargantext.Database.Query.Table.NodeNgrams (queryNodeNgramsTable)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNgrams
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import qualified Database.PostgreSQL.Simple as PGS

queryNgramsTable :: Select NgramsRead
queryNgramsTable = selectTable ngramsTable

selectNgramsByDoc :: [ListId] -> DocId -> NgramsType -> Cmd err [Text]
selectNgramsByDoc lIds dId nt = runOpaQuery (query lIds dId nt)
  where

    join :: Select (NgramsRead, NodeNgramsRead, ContextNodeNgrams2Read)
    join = leftJoin3 queryNgramsTable queryNodeNgramsTable queryContextNodeNgrams2Table on1 -- on2
      where
        on1 :: (NgramsRead, NodeNgramsRead, ContextNodeNgrams2Read) -> Column SqlBool
        on1 (ng, nng, cnng) =  (.&&)
                                 (ng^.ngrams_id .== nng^.nng_ngrams_id)
                                 (nng^.nng_id   .== cnng^.cnng2_nodengrams_id)

    query lIds' dId' nt' = proc () -> do
      (ng,nng,cnng) <- join -< ()
      restrict -< foldl (\b lId -> ((pgNodeId lId) .== nng^.nng_node_id) .|| b) (sqlBool True) lIds'
      restrict -< (pgNodeId dId')    .== cnng^.cnng2_context_id
      restrict -< (pgNgramsType nt') .== nng^.nng_ngrams_type
      returnA  -< ng^.ngrams_terms


_postNgrams :: CorpusId -> DocId -> [Text] -> Cmd err Int
_postNgrams = undefined

_dbGetNgramsDb :: Cmd err [NgramsDB]
_dbGetNgramsDb = runOpaQuery queryNgramsTable


-- TODO-ACCESS: access must not be checked here but when insertNgrams is called.
insertNgrams :: [Ngrams] -> Cmd err (HashMap Text NgramsId)
insertNgrams ns =
  if List.null ns
     then pure HashMap.empty
     else HashMap.fromList <$> map (\(Indexed i t) -> (t, i)) <$> (insertNgrams' ns)

-- TODO-ACCESS: access must not be checked here but when insertNgrams' is called.
insertNgrams' :: [Ngrams] -> Cmd err [Indexed Int Text]
insertNgrams' ns = runPGSQuery queryInsertNgrams (PGS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

_insertNgrams_Debug :: [(Text, Size)] -> Cmd err ByteString
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


--------------------------------------------------------------------------
selectNgramsId :: [Text] -> Cmd err (Map NgramsId Text)
selectNgramsId ns =
  if List.null ns
     then pure Map.empty
     else Map.fromList <$> map (\(Indexed i t) -> (i, t)) <$> (selectNgramsId' ns)

selectNgramsId' :: [Text] -> Cmd err [Indexed Int Text]
selectNgramsId' ns = runPGSQuery querySelectNgramsId ( PGS.Only
                                                     $ Values fields ns
                                                     )
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text"]

    querySelectNgramsId :: PGS.Query
    querySelectNgramsId = [sql|
        WITH input_rows(terms) AS (?)
        SELECT n.id, n.terms
        FROM   ngrams n
        JOIN input_rows ir ON ir.terms = n.terms
        GROUP BY n.terms, n.id
        |]
