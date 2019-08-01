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
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.TextSearch where

import Data.Aeson
import Data.Map.Strict hiding (map, drop, take)
import Data.Maybe
import Data.List (intersperse, take, drop)
import Data.String (IsString(..))
import Data.Text (Text, words, unpack, intercalate)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Prelude
--import Gargantext.Database.Node.Contact
import Gargantext.Database.Facet
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNode hiding (joinInCorpus)
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Database.Queries.Join (leftJoin6)
import Gargantext.Database.Utils (Cmd, runPGSQuery, runOpaQuery)
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import Gargantext.Core.Types
import Control.Arrow (returnA)
import qualified Opaleye as O hiding (Order)
import Opaleye hiding (Query, Order)


------------------------------------------------------------------------
searchInDatabase :: ParentId -> Text -> Cmd err [(NodeId, HyperdataDocument)]
searchInDatabase p t = runOpaQuery (queryInDatabase p t)

-- | Global search query where ParentId is Master Node Corpus Id 
queryInDatabase :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
queryInDatabase _ q = proc () -> do
    row <- queryNodeSearchTable -< ()
    restrict -< (_ns_search row)    @@ (pgTSQuery (unpack q))
    restrict -< (_ns_typename row) .== (pgInt4 $ nodeTypeId NodeDocument)
    returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | todo add limit and offset and order
searchInCorpus :: CorpusId -> IsTrash -> [Text] -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err [FacetDoc]
searchInCorpus cId t q o l order = runOpaQuery (filterWith o l order $ queryInCorpus cId t q')
  where
    q' = intercalate " | " $ map stemIt q

queryInCorpus :: CorpusId -> IsTrash -> Text -> O.Query FacetDocRead
queryInCorpus cId t q = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< ( nn_node1_id nn) .== (toNullable $ pgNodeId cId)
  restrict -< if t
                 then ( nn_category nn) .== (toNullable $ pgInt4     0)
                 else ( nn_category nn) .>= (toNullable $ pgInt4     1)
  restrict -< (_ns_search n)           @@ (pgTSQuery (unpack q))
  restrict -< (_ns_typename n)        .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA  -< FacetDoc (_ns_id n) (_ns_date n) (_ns_name n) (_ns_hyperdata n) (nn_category nn) (nn_score nn)

joinInCorpus :: O.Query (NodeSearchRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeSearchTable queryNodeNodeTable cond
  where
    cond :: (NodeSearchRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nn_node2_id nn .== _ns_id n

------------------------------------------------------------------------
type AuthorName = Text

-- | TODO Optim: Offset and Limit in the Query
searchInCorpusWithContacts :: CorpusId -> [Text] -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err [FacetPaired Int UTCTime HyperdataDocument Int [Pair Int Text]]
searchInCorpusWithContacts cId q o l order = take (maybe 5 identity l) <$> drop (maybe 0 identity o)
  <$> map (\((i,u,h,s), ps) -> FacetPaired i u h s (catMaybes ps))
  <$> toList <$> fromListWith (<>)
  <$> map (\(FacetPaired i u h s p) -> ((i,u,h,s), [maybePair p]))
  <$> searchInCorpusWithContacts' cId q o l order
  where
    maybePair (Pair Nothing Nothing) = Nothing
    maybePair (Pair _ Nothing) = Nothing
    maybePair (Pair Nothing _) = Nothing
    maybePair (Pair (Just p_id) (Just p_label)) = Just $ Pair p_id p_label

searchInCorpusWithContacts' :: CorpusId -> [Text] -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err [(FacetPaired Int UTCTime HyperdataDocument Int (Pair (Maybe Int) (Maybe Text)))]
searchInCorpusWithContacts' cId q o l order = runOpaQuery $ queryInCorpusWithContacts cId q' o l order
  where
    q' = intercalate " | " $ map stemIt q



queryInCorpusWithContacts :: CorpusId -> Text -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> O.Query FacetPairedRead
queryInCorpusWithContacts cId q _ _ _ = proc () -> do
  (docs, (corpusDoc, (_docNgrams, (ngrams', (_, contacts))))) <- joinInCorpusWithContacts -< ()
  restrict -< (_ns_search docs)              @@ (pgTSQuery  $ unpack q  )
  restrict -< (_ns_typename docs)           .== (pgInt4 $ nodeTypeId NodeDocument)
  restrict -< (nn_node1_id corpusDoc) .== (toNullable $ pgNodeId cId)
  -- restrict -< (nng_listType docNgrams)      .== (toNullable $ pgNgramsType Authors)
  restrict -< (_node_typename contacts)     .== (toNullable $ pgInt4 $ nodeTypeId NodeContact)
  -- let contact_id    = ifThenElse (isNull $ _node_id contacts) (toNullable $ pgInt4 0) (_node_id contacts)
  returnA  -< FacetPaired (_ns_id docs) (_ns_date docs) (_ns_hyperdata docs) (pgInt4 0) (Pair (_node_id contacts) (ngrams_terms ngrams'))

joinInCorpusWithContacts :: O.Query (NodeSearchRead, (NodeNodeReadNull, (NodeNodeNgramsReadNull, (NgramsReadNull, (NodeNodeNgramsReadNull, NodeReadNull)))))
joinInCorpusWithContacts = leftJoin6 queryNodeTable queryNodeNodeNgramsTable queryNgramsTable queryNodeNodeNgramsTable queryNodeNodeTable queryNodeSearchTable cond12 cond23 cond34 cond45 cond56
    where
         cond12 :: (NodeNodeNgramsRead, NodeRead) -> Column PGBool
         cond12 (ng3, n2) = _node_id n2 .== nnng_node1_id ng3
---------
         cond23 :: (NgramsRead, (NodeNodeNgramsRead, NodeReadNull)) -> Column PGBool
         cond23 (ng2, (nnng2, _)) = nnng_ngrams_id nnng2 .== ngrams_id ng2
         
         cond34 :: (NodeNodeNgramsRead, (NgramsRead, (NodeNodeNgramsReadNull, NodeReadNull))) -> Column PGBool
         cond34 (nng, (ng, (_,_))) = ngrams_id ng .== nnng_ngrams_id nng
         
         cond45 :: (NodeNodeRead, (NodeNodeNgramsRead, (NgramsReadNull, (NodeNodeNgramsReadNull, NodeReadNull)))) -> Column PGBool
         cond45 (nn, (nng, (_,(_,_)))) = nnng_node1_id nng .== nn_node2_id nn
         
         cond56 :: (NodeSearchRead, (NodeNodeRead, (NodeNodeNgramsReadNull, (NgramsReadNull, (NodeNodeNgramsReadNull, NodeReadNull))))) -> Column PGBool
         cond56 (n, (nn, (_,(_,(_,_))))) = _ns_id n .== nn_node2_id nn




newtype TSQuery = UnsafeTSQuery [Text]

-- | TODO [""] -> panic "error"
toTSQuery :: [Text] -> TSQuery
toTSQuery txt = UnsafeTSQuery $ map stemIt txt


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
-- textSearchTest :: ParentId -> TSQuery -> Cmd err [(Int, Value, Value, Value, Value, Maybe Int)]
-- textSearchTest pId q = textSearch q pId 5 0 Asc
textSearch :: TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> Cmd err [(Int,Value,Value,Value, Value, Maybe Int)]
textSearch q p l o ord = runPGSQuery textSearchQuery (q,p,p,typeId,ord,o,l)
  where
    typeId = nodeTypeId NodeDocument


