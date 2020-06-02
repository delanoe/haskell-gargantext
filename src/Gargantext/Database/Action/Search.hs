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

module Gargantext.Database.Action.Search where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Aeson
import Data.List (intersperse, take, drop)
import Data.Map.Strict hiding (map, drop, take)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text, words, unpack, intercalate)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField
import Gargantext.Core.Types
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Join (leftJoin6)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.NodeNode
import Gargantext.Database.Query.Table.NodeNodeNgrams
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Prelude (Cmd, runPGSQuery, runOpaQuery, runCountOpaQuery)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import Opaleye hiding (Query, Order)
import qualified Opaleye as O hiding (Order)

------------------------------------------------------------------------
searchInDatabase :: ParentId
                 -> Text
                 -> Cmd err [(NodeId, HyperdataDocument)]
searchInDatabase p t = runOpaQuery (queryInDatabase p t)
  where
    -- | Global search query where ParentId is Master Node Corpus Id 
    queryInDatabase :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
    queryInDatabase _ q = proc () -> do
        row <- queryNodeSearchTable -< ()
        restrict -< (_ns_search row)    @@ (pgTSQuery (unpack q))
        restrict -< (_ns_typename row) .== (pgInt4 $ nodeTypeId NodeDocument)
        returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | todo add limit and offset and order
searchInCorpus :: CorpusId
               -> IsTrash
               -> [Text]
               -> Maybe Offset
               -> Maybe Limit
               -> Maybe OrderBy
               -> Cmd err [FacetDoc]
searchInCorpus cId t q o l order = runOpaQuery
                                 $ filterWith o l order
                                 $ queryInCorpus cId t
                                 $ intercalate " | "
                                 $ map stemIt q

searchCountInCorpus :: CorpusId
                    -> IsTrash
                    -> [Text]
                    -> Cmd err Int
searchCountInCorpus cId t q = runCountOpaQuery
                            $ queryInCorpus cId t
                            $ intercalate " | "
                            $ map stemIt q

queryInCorpus :: CorpusId
              -> IsTrash
              -> Text
              -> O.Query FacetDocRead
queryInCorpus cId t q = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< (nn^.nn_node1_id) .== (toNullable $ pgNodeId cId)
  restrict -< if t
                 then (nn^.nn_category) .== (toNullable $ pgInt4 0)
                 else (nn^.nn_category) .>= (toNullable $ pgInt4 1)
  restrict -< (n ^. ns_search)           @@ (pgTSQuery (unpack q))
  restrict -< (n ^. ns_typename )       .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA  -< FacetDoc (n^.ns_id       )
                       (n^.ns_date     )
                       (n^.ns_name     )
                       (n^.ns_hyperdata)
                       (nn^.nn_category)
                       (nn^.nn_score   )

joinInCorpus :: O.Query (NodeSearchRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeSearchTable queryNodeNodeTable cond
  where
    cond :: (NodeSearchRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nn^.nn_node2_id .== _ns_id n

------------------------------------------------------------------------
type AuthorName = Text

-- | TODO Optim: Offset and Limit in the Query
-- TODO-SECURITY check
searchInCorpusWithContacts
  :: CorpusId
  -> ListId
  -> [Text]
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe OrderBy
  -> Cmd err [FacetPaired Int UTCTime HyperdataDocument Int [Pair Int Text]]
searchInCorpusWithContacts cId lId q o l order =
      take (maybe 10 identity l)
  <$> drop (maybe 0 identity o)
  <$> map (\((i,u,h,s), ps) -> FacetPaired i u h s ps)
  <$> toList <$> fromListWith (<>)
  <$> map (\(FacetPaired i u h s (p1,p2)) -> ( (i,u,h,s)
                                             , catMaybes [Pair <$> p1 <*> p2]
                                             )
          )
  <$> searchInCorpusWithContacts' cId lId q o l order

-- TODO-SECURITY check
searchInCorpusWithContacts'
  :: CorpusId
  -> ListId
  -> [Text]
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe OrderBy
  -> Cmd err [(FacetPaired Int UTCTime HyperdataDocument Int (Maybe Int, Maybe Text))]
searchInCorpusWithContacts' cId lId q o l order =
  runOpaQuery $ queryInCorpusWithContacts cId lId o l order
              $ intercalate " | "
              $ map stemIt q


queryInCorpusWithContacts
  :: CorpusId
  -> ListId
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe OrderBy
  -> Text
  -> O.Query FacetPairedRead
queryInCorpusWithContacts cId _lId _ _ _ q = proc () -> do
  (n, (nn, (_nng, (ngrams', (_, contacts))))) <- joinInCorpusWithContacts -< ()
  restrict -< (n^.ns_search)        @@ (pgTSQuery  $ unpack q  )
  restrict -< (n^.ns_typename)     .== (pgInt4 $ nodeTypeId NodeDocument)
--  restrict -< (nng^.nnng_node1_id) .== (toNullable $ pgNodeId lId)
  restrict -< (nn^.nn_node1_id)    .== (toNullable $ pgNodeId cId)
--   -- restrict -< (nng_listType nng)      .== (toNullable $ pgNgramsType Authors)
--  restrict -< (contacts^.node_typename) .== (toNullable $ pgInt4 $ nodeTypeId NodeContact)
--   -- let contact_id    = ifThenElse (isNull $ _node_id contacts) (toNullable $ pgInt4 0) (_node_id contacts)
  returnA  -< FacetPaired (n^.ns_id)
                          (n^.ns_date)
                          (n^.ns_hyperdata)
                          (pgInt4 0)
                          (contacts^.node_id, ngrams'^.ngrams_terms)

joinInCorpusWithContacts :: O.Query ( NodeSearchRead
                                    , ( NodeNodeReadNull
                                      , ( NodeNodeNgramsReadNull
                                        , ( NgramsReadNull
                                          , ( NodeNodeNgramsReadNull
                                            , NodeReadNull
                                            )
                                          )
                                        )
                                      )
                                    )
joinInCorpusWithContacts =
  leftJoin6
  queryNodeTable
  queryNodeNodeNgramsTable
  queryNgramsTable
  queryNodeNodeNgramsTable
  queryNodeNodeTable
  queryNodeSearchTable
  cond12
  cond23
  cond34
  cond45
  cond56
    where
      cond12 :: (NodeNodeNgramsRead, NodeRead) -> Column PGBool
      cond12 (nnng, n2) = n2^.node_id .== nnng^.nnng_node1_id

      cond23 :: (NgramsRead, (NodeNodeNgramsRead, NodeReadNull)) -> Column PGBool
      cond23 (ng2, (nnng2, _)) = nnng2^.nnng_ngrams_id .== ng2^.ngrams_id

      cond34 :: ( NodeNodeNgramsRead
                , ( NgramsRead
                  , ( NodeNodeNgramsReadNull
                    , NodeReadNull
                    )
                  )
                ) -> Column PGBool
      cond34 (nng, (ng, (_,_))) = ng^.ngrams_id .== nng^.nnng_ngrams_id
 
      cond45 :: ( NodeNodeRead
                , ( NodeNodeNgramsRead
                  , ( NgramsReadNull
                    , ( NodeNodeNgramsReadNull
                      , NodeReadNull
                      )
                    )
                  )
                ) -> Column PGBool
      cond45 (nn, (nng, (_,(_,_)))) = nng^.nnng_node1_id .== nn^.nn_node2_id
 
      cond56 :: ( NodeSearchRead
                , ( NodeNodeRead
                  , ( NodeNodeNgramsReadNull
                    , ( NgramsReadNull
                      , ( NodeNodeNgramsReadNull
                        , NodeReadNull
                        )
                      )
                    )
                  )
                ) -> Column PGBool
      cond56 (n, (nn, (_,(_,(_,_))))) = _ns_id n .== nn^.nn_node2_id
 

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


