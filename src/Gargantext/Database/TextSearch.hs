{-|
Module      : Gargantext.Database.TextSearch
Description : Postgres text search experimentation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.TextSearch where

import Data.Aeson
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text, words, unpack)
import Database.PostgreSQL.Simple -- (Query, Connection)
import Database.PostgreSQL.Simple.ToField
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Prelude
import Gargantext.Database.Node.Contact
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Schema.NodeNgram
import Gargantext.Database.Queries.Join (leftJoin6, leftJoin3')
import Gargantext.Core.Types
import Control.Arrow (returnA)
import qualified Opaleye as O hiding (Order)
import Opaleye hiding (Query, Order)


------------------------------------------------------------------------
searchInDatabase :: Connection -> ParentId -> Text -> IO [(NodeId, HyperdataDocument)]
searchInDatabase c p t = runQuery c (queryInDatabase p t)

-- | Global search query where ParentId is Master Node Corpus Id 
queryInDatabase :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
queryInDatabase _ q = proc () -> do
    row <- queryNodeSearchTable -< ()
    restrict -< (_ns_search row)    @@ (pgTSQuery (unpack q))
    restrict -< (_ns_typename row) .== (pgInt4 $ nodeTypeId NodeDocument)
    returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | todo add limit and offset and order
searchInCorpus :: Connection -> CorpusId -> Text -> IO [(NodeId, HyperdataDocument)]
searchInCorpus c cId q = runQuery c (queryInCorpus cId q)

queryInCorpus :: CorpusId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
queryInCorpus cId q = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< ( nodeNode_node1_id nn) .== (toNullable $ pgInt4 cId)
  restrict -< (_ns_search n)           @@ (pgTSQuery (unpack q))
  restrict -< (_ns_typename n)        .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA  -< (_ns_id n, _ns_hyperdata n)

joinInCorpus :: O.Query (NodeSearchRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeSearchTable queryNodeNodeTable cond
  where
    cond :: (NodeSearchRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nodeNode_node2_id nn .== _ns_id n

------------------------------------------------------------------------
getGraphCorpusAuthors :: Connection -> CorpusId -> Text -> IO [((Int, HyperdataDocument),(Int, Maybe Text))]
getGraphCorpusAuthors c cId q = runQuery c $ selectGraphCorpusAuthors cId q

selectGraphCorpusAuthors :: CorpusId -> Text -> O.Query ((Column PGInt4, Column PGJsonb), (Column (PGInt4), Column (Nullable PGText)))
selectGraphCorpusAuthors cId q = proc () -> do
  (docs, (corpusDoc, (docNgrams, (ngrams, (ngramsContact, contacts))))) <- queryGraphCorpusAuthors -< ()
  restrict -< (_ns_search docs)              @@ (pgTSQuery  $ unpack q  )
  restrict -< (_ns_typename docs)           .== (pgInt4 $ nodeTypeId NodeDocument)
  restrict -< (nodeNode_node1_id corpusDoc) .== (toNullable $ pgInt4 cId)
  restrict -< (nodeNgram_type docNgrams)    .== (toNullable $ pgInt4 $ ngramsTypeId Authors)
  restrict -< (_node_typename contacts)     .== (toNullable $ pgInt4 $ nodeTypeId NodeContact)
  -- let contact_id    = ifThenElse (isNull $ _node_id contacts) (toNullable $ pgInt4 0) (_node_id contacts)
  returnA  -< ((_ns_id docs, _ns_hyperdata docs),(fromNullable (pgInt4 0) (_node_id contacts), ngrams_terms ngrams))


queryGraphCorpusAuthors :: O.Query (NodeSearchRead, (NodeNodeReadNull, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))))
queryGraphCorpusAuthors = leftJoin6 queryNodeTable queryNodeNgramTable queryNgramsTable queryNodeNgramTable queryNodeNodeTable queryNodeSearchTable cond12 cond23 cond34 cond45 cond56
    where
         cond12 :: (NodeNgramRead, NodeRead) -> Column PGBool
         cond12 (ng3, n2) = _node_id n2 .== nodeNgram_node_id ng3
---------
         cond23 :: (NgramsRead, (NodeNgramRead, NodeReadNull)) -> Column PGBool
         cond23 (ng2, (nng2, _)) = nodeNgram_ngrams_id nng2 .== ngrams_id ng2
         
         cond34 :: (NodeNgramRead, (NgramsRead, (NodeNgramReadNull, NodeReadNull))) -> Column PGBool
         cond34 (nng, (ng, (_,_))) = ngrams_id ng .== nodeNgram_ngrams_id nng
         
         cond45 :: (NodeNodeRead, (NodeNgramRead, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))) -> Column PGBool
         cond45 (nn, (nng, (_,(_,_)))) = nodeNgram_node_id nng .== nodeNode_node2_id nn
         
         cond56 :: (NodeSearchRead, (NodeNodeRead, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull))))) -> Column PGBool
         cond56 (n, (nn, (_,(_,(_,_))))) = _ns_id n .== nodeNode_node2_id nn


{-
queryGraphCorpusAuthors' :: O.Query (NodeSearchRead, (NodeNodeReadNull, (NodeNgramReadNull, NgramsReadNull)))
queryGraphCorpusAuthors' = leftJoin4 queryNgramsTable queryNodeNgramTable queryNodeNodeTable queryNodeSearchTable cond12 cond23 cond34
    where
         cond23 :: (NgramsRead, (NodeNgramRead, NodeReadNull)) -> Column PGBool
         cond23 (ng2, (nng2, _)) = nodeNgram_ngrams_id nng2 .== ngrams_id ng2
         
         cond34 :: (NodeNgramRead, (NgramsRead, (NodeNgramReadNull, NodeReadNull))) -> Column PGBool
         cond34 (nng, (ng, (_,_))) = ngrams_id ng .== nodeNgram_ngrams_id nng
         
         cond45 :: (NodeNodeRead, (NodeNgramRead, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))) -> Column PGBool
         cond45 (nn, (nng, (_,(_,_)))) = nodeNgram_node_id nng .== nodeNode_node2_id nn
         
         cond56 :: (NodeSearchRead, (NodeNodeRead, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull))))) -> Column PGBool
         cond56 (n, (nn, (_,(_,(_,_))))) = _ns_id n .== nodeNode_node2_id nn
-}



newtype TSQuery = UnsafeTSQuery [Text]

-- | TODO [""] -> panic "error"
toTSQuery :: [Text] -> TSQuery
toTSQuery txt = UnsafeTSQuery txt


instance IsString TSQuery
  where
    fromString = UnsafeTSQuery . words . cs


instance ToField TSQuery
  where
    toField (UnsafeTSQuery xs)
      = Many  $ intersperse (Plain " && ")
              $ map (\q -> Many [ Plain "plainto_tsquery("
                                , Escape (cs q)
                                , Plain ")"
                                ]
                    ) xs

data Order    = Asc | Desc

instance ToField Order
  where
    toField Asc  = Plain "ASC"
    toField Desc = Plain "DESC"

-- TODO
-- FIX fav
-- ADD ngrams count
-- TESTS
textSearchQuery :: Query
textSearchQuery = "SELECT n.id, n.hyperdata->'publication_year'     \
\                   , n.hyperdata->'title'                          \
\                   , n.hyperdata->'source'                         \
\                   , n.hyperdata->'authors'                        \
\                   , COALESCE(nn.score,null)                       \
\                      FROM nodes n                                 \
\            LEFT JOIN nodes_nodes nn  ON nn.node2_id = n.id        \
\              WHERE                                                \
\                n.search @@ (?::tsquery)                           \
\                AND (n.parent_id = ? OR nn.node1_id = ?)           \
\                AND n.typename  = ?                                \
\                ORDER BY n.hyperdata -> 'publication_date' ?       \
\            offset ? limit ?;"

-- | Text Search Function for Master Corpus
-- TODO : text search for user corpus
-- Example:
-- textSearchTest :: ParentId -> TSQuery -> Cmd [(Int, Value, Value, Value, Value, Maybe Int)]
-- textSearchTest pId q = mkCmd $ \c -> textSearch c q pId 5 0 Asc
textSearch :: Connection 
           -> TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> IO [(Int,Value,Value,Value, Value, Maybe Int)]
textSearch conn q p l o ord = query conn textSearchQuery (q,p,p,typeId,ord,o,l)
  where
    typeId = nodeTypeId NodeDocument


