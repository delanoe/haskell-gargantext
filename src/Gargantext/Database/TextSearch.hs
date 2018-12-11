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

newtype TSQuery = UnsafeTSQuery [Text]

globalTextSearch :: Connection -> ParentId -> Text -> IO [(NodeId, HyperdataDocument)]
globalTextSearch c p t = runQuery c (globalTextSearchQuery p t)

-- | Global search query where ParentId is Master Node Corpus Id 
globalTextSearchQuery :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
globalTextSearchQuery _ q = proc () -> do
    row <- queryNodeSearchTable -< ()
    restrict -< (_ns_search row)    @@ (pgTSQuery (unpack q))
    restrict -< (_ns_typename row) .== (pgInt4 $ nodeTypeId NodeDocument)
    returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | todo add limit and offset and order
graphCorpusDocSearch :: CorpusId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
graphCorpusDocSearch cId q = proc () -> do
  (n, nn) <- graphCorpusDocSearchQuery -< ()
  restrict -< (_ns_search n) @@ (pgTSQuery (unpack q))
  restrict -< ( nodeNode_node1_id nn) .== (toNullable $ pgInt4 cId)
  restrict -< (_ns_typename n) .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA  -< (_ns_id n, _ns_hyperdata n)

graphCorpusDocSearchQuery :: O.Query (NodeSearchRead, NodeNodeReadNull)
graphCorpusDocSearchQuery = leftJoin queryNodeSearchTable queryNodeNodeTable cond
  where
    cond :: (NodeSearchRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nodeNode_node1_id nn .== _ns_id n


getGraphCorpusAuthors :: Connection -> CorpusId -> Text -> IO [((Int, HyperdataDocument),(Maybe Int, Maybe HyperdataContact))]
getGraphCorpusAuthors c cId q = runQuery c $ selectGraphCorpusAuthors' cId q

selectGraphCorpusAuthors' :: CorpusId -> Text -> O.Query ((Column PGInt4, Column PGJsonb),(Column (Nullable PGInt4), Column (Nullable PGJsonb)))
selectGraphCorpusAuthors' cId q = proc () -> do
  (docs, (corpusDoc, (docNgrams, (ngrams, (ngramsContact, contacts))))) <- queryGraphCorpusAuthors' -< ()
  restrict -< (_ns_search docs)              @@ (pgTSQuery  $ unpack q  )
  restrict -< (nodeNode_node1_id corpusDoc) .== (toNullable $ pgInt4 cId)
  restrict -< (_ns_typename docs)           .== (pgInt4 $ nodeTypeId NodeDocument)
  restrict -< (nodeNgram_type docNgrams)    .== (toNullable $ pgInt4 $ ngramsTypeId Authors)
  restrict -< (_node_typename contacts)     .== (toNullable $ pgInt4 $ nodeTypeId NodeContact)
  returnA  -< ((_ns_id docs, _ns_hyperdata docs), (_node_id contacts, _node_hyperdata contacts))



-- | This query can be used to select document with Authors in Annuaire only
selectGraphCorpusAuthors :: CorpusId -> Text -> O.Query (Column (Nullable PGInt4), Column PGInt4, Column PGJsonb)
selectGraphCorpusAuthors cId q = proc () -> do
  (contacts, (contactNgrams, (ngrams, (docNgrams, (corpusDoc, docSearch))))) <- queryGraphCorpusAuthors -< ()
  restrict -< (_ns_search docSearch)         @@ (pgTSQuery  $ unpack q  )
  restrict -< (nodeNode_node1_id corpusDoc) .== (toNullable $ pgInt4 cId)
  restrict -< (_ns_typename docSearch)      .== (toNullable $ pgInt4 $ nodeTypeId NodeDocument)
  restrict -< (nodeNgram_type docNgrams)    .== (toNullable $ pgInt4 $ ngramsTypeId Authors)
  restrict -< (_node_typename contacts)     .== (pgInt4 $ nodeTypeId NodeContact)
  returnA  -< (_ns_id docSearch, _node_id contacts, _node_hyperdata contacts)
  --returnA  -< (_ns_id docSearch, _ns_name docSearch)


queryGraphCorpusAuthors :: O.Query (NodeRead, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, (NodeNodeReadNull, NodeSearchReadNull)))))
queryGraphCorpusAuthors = leftJoin6 queryNodeSearchTable queryNodeNodeTable queryNodeNgramTable queryNgramsTable queryNodeNgramTable queryNodeTable cond12 cond23 cond34 cond45 cond56
    where
         cond12 :: (NodeNodeRead, NodeSearchRead) -> Column PGBool
         cond12 (nn, n) = nodeNode_node2_id nn .== _ns_id n
         
         cond23 :: (NodeNgramRead, (NodeNodeRead, NodeSearchReadNull)) -> Column PGBool
         cond23 (nng, (nn, _)) = nodeNgram_node_id nng .== nodeNode_node2_id nn
         
         cond34 :: (NgramsRead, (NodeNgramRead, (NodeNodeReadNull, NodeSearchReadNull))) -> Column PGBool
         cond34 (ng, (nng, (_,_))) = ngrams_id ng .== nodeNgram_ngrams_id nng
         
         cond45 :: (NodeNgramRead, (NgramsRead, (NodeNgramReadNull, (NodeNodeReadNull, NodeSearchReadNull)))) -> Column PGBool
         cond45 (nng2, (ng2, (_,(_,_)))) = nodeNgram_ngrams_id nng2 .== ngrams_id ng2
         
         cond56 :: (NodeRead, (NodeNgramRead, (NgramsReadNull, (NodeNgramReadNull, (NodeNodeReadNull, NodeSearchReadNull))))) -> Column PGBool
         cond56 (n2, (ng3, (_,(_,(_,_))))) = _node_id n2 .== nodeNgram_node_id ng3


queryGraphCorpusAuthors' :: O.Query (NodeSearchRead, (NodeNodeReadNull, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))))
queryGraphCorpusAuthors' = leftJoin6 queryNodeTable queryNodeNgramTable queryNgramsTable queryNodeNgramTable queryNodeNodeTable queryNodeSearchTable cond12 cond23 cond34 cond45 cond56
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


