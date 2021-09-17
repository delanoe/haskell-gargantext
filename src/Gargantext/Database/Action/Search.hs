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
import Data.List (intersperse)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text, words, unpack, intercalate)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..), HyperdataContact(..))
import Gargantext.Database.Prelude (Cmd, runPGSQuery, runOpaQuery, runCountOpaQuery)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Filter
import Gargantext.Database.Query.Join (leftJoin5)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.NodeNode
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Gargantext.Core.Text.Terms.Mono.Stem.En (stemIt)
import Opaleye hiding (Query, Order)
import Data.Profunctor.Product (p4)
import qualified Opaleye as O hiding (Order)

------------------------------------------------------------------------
searchDocInDatabase :: HasDBid NodeType
                    => ParentId
                    -> Text
                    -> Cmd err [(NodeId, HyperdataDocument)]
searchDocInDatabase p t = runOpaQuery (queryDocInDatabase p t)
  where
    -- | Global search query where ParentId is Master Node Corpus Id 
    queryDocInDatabase :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
    queryDocInDatabase _ q = proc () -> do
        row <- queryNodeSearchTable -< ()
        restrict -< (_ns_search row)    @@ (pgTSQuery (unpack q))
        restrict -< (_ns_typename row) .== (pgInt4 $ toDBid NodeDocument)
        returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | todo add limit and offset and order
searchInCorpus :: HasDBid NodeType
               => CorpusId
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

searchCountInCorpus :: HasDBid NodeType
                    => CorpusId
                    -> IsTrash
                    -> [Text]
                    -> Cmd err Int
searchCountInCorpus cId t q = runCountOpaQuery
                            $ queryInCorpus cId t
                            $ intercalate " | "
                            $ map stemIt q

queryInCorpus :: HasDBid NodeType
              => CorpusId
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
  restrict -< (n ^. ns_typename )       .== (pgInt4 $ toDBid NodeDocument)
  returnA  -< FacetDoc { facetDoc_id = n^.ns_id        
                       , facetDoc_created = n^.ns_date
                       , facetDoc_title = n^.ns_name
                       , facetDoc_hyperdata = n^.ns_hyperdata
                       , facetDoc_category = nn^.nn_category
                       , facetDoc_ngramCount = nn^.nn_score
                       , facetDoc_score = nn^.nn_score }

joinInCorpus :: O.Query (NodeSearchRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeSearchTable queryNodeNodeTable cond
  where
    cond :: (NodeSearchRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nn^.nn_node2_id .== _ns_id n

------------------------------------------------------------------------
searchInCorpusWithContacts
  :: HasDBid NodeType
  => CorpusId
  -> AnnuaireId
  -> [Text]
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe OrderBy
  -> Cmd err [FacetPaired Int UTCTime HyperdataContact Int]
searchInCorpusWithContacts cId aId q o l _order =
  runOpaQuery $ limit'   l
              $ offset'  o
              $ orderBy ( desc _fp_score)
              $ selectGroup cId aId
              $ intercalate " | "
              $ map stemIt q

selectContactViaDoc
  :: HasDBid NodeType
  => CorpusId
  -> AnnuaireId
  -> Text
  -> QueryArr ()
              ( Column (Nullable PGInt4)
              , Column (Nullable PGTimestamptz)
              , Column (Nullable PGJsonb)
              , Column (Nullable PGInt4)
              )
selectContactViaDoc cId aId q = proc () -> do
  (doc, (corpus_doc, (_contact_doc, (annuaire_contact, contact)))) <- queryContactViaDoc -< ()
  restrict -< (doc^.ns_search)           @@ (pgTSQuery  $ unpack q  )
  restrict -< (doc^.ns_typename)        .== (pgInt4 $ toDBid NodeDocument)
  restrict -< (corpus_doc^.nn_node1_id)  .== (toNullable $ pgNodeId cId)
  restrict -< (annuaire_contact^.nn_node1_id) .== (toNullable $ pgNodeId aId)
  restrict -< (contact^.node_typename)        .== (toNullable $ pgInt4 $ toDBid NodeContact)
  returnA  -< ( contact^.node_id
              , contact^.node_date
              , contact^.node_hyperdata
              , toNullable $ pgInt4 1
              )

selectGroup :: HasDBid NodeType
            => NodeId
            -> NodeId
            -> Text
            -> Select FacetPairedReadNull
selectGroup cId aId q = proc () -> do
  (a, b, c, d) <- aggregate (p4 (groupBy, groupBy, groupBy, O.sum))
                            (selectContactViaDoc cId aId q) -< ()
  returnA -< FacetPaired { _fp_id = a
                         , _fp_date = b
                         , _fp_hyperdata = c
                         , _fp_score = d }


queryContactViaDoc :: O.Query ( NodeSearchRead
                              , ( NodeNodeReadNull
                                , ( NodeNodeReadNull
                                  , ( NodeNodeReadNull
                                    , NodeReadNull
                                    )
                                  )
                                )
                              )
queryContactViaDoc =
  leftJoin5
  queryNodeTable
  queryNodeNodeTable
  queryNodeNodeTable
  queryNodeNodeTable
  queryNodeSearchTable
  cond12
  cond23
  cond34
  cond45
    where
      cond12 :: (NodeNodeRead, NodeRead) -> Column PGBool
      cond12 (annuaire_contact, contact) = contact^.node_id .== annuaire_contact^.nn_node2_id

      cond23 :: ( NodeNodeRead
                , ( NodeNodeRead
                  , NodeReadNull
                  )
                ) -> Column PGBool
      cond23 (contact_doc, (annuaire_contact, _)) = contact_doc^.nn_node1_id .== annuaire_contact^.nn_node2_id

      cond34 :: ( NodeNodeRead
                , ( NodeNodeRead
                  , ( NodeNodeReadNull
                    , NodeReadNull
                    )
                  )
                ) -> Column PGBool
      cond34 (corpus_doc, (contact_doc, (_,_))) =  corpus_doc^.nn_node2_id .== contact_doc^.nn_node2_id


      cond45 :: ( NodeSearchRead
                , ( NodeNodeRead
                  , ( NodeNodeReadNull
                    , ( NodeNodeReadNull
                      , NodeReadNull
                      )
                    )
                  )
                ) -> Column PGBool
      cond45 (doc, (corpus_doc, (_,(_,_)))) = doc^.ns_id .== corpus_doc^.nn_node2_id


------------------------------------------------------------------------

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
textSearch :: HasDBid NodeType
           => TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> Cmd err [(Int, Value, Value, Value, Value, Maybe Int)]
textSearch q p l o ord = runPGSQuery textSearchQuery (q,p,p,typeId,ord,o,l)
  where
    typeId = toDBid NodeDocument


