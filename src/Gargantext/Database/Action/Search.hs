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
{-# LANGUAGE LambdaCase         #-}

module Gargantext.Database.Action.Search where

import Control.Arrow (returnA)
import Control.Lens ((^.), view)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, unpack, intercalate)
import Data.Time (UTCTime)
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..), HyperdataContact(..))
import Gargantext.Database.Prelude (Cmd, runOpaQuery, runCountOpaQuery)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Filter
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Context
import Gargantext.Database.Query.Table.ContextNodeNgrams (queryContextNodeNgramsTable)
import Gargantext.Database.Query.Table.NodeContext
import Gargantext.Database.Query.Table.NodeContext_NodeContext
import Gargantext.Database.Schema.ContextNodeNgrams (ContextNodeNgramsPoly(..))
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Context
import Gargantext.Prelude
import Gargantext.Core.Text.Terms.Mono.Stem.En (stemIt)
import Opaleye hiding (Order)
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
    queryDocInDatabase :: ParentId -> Text -> O.Select (Column SqlInt4, Column SqlJsonb)
    queryDocInDatabase _p q = proc () -> do
        row <- queryNodeSearchTable -< ()
        restrict -< (_ns_search row)    @@ (sqlTSQuery (unpack q))
        restrict -< (_ns_typename row) .== (sqlInt4 $ toDBid NodeDocument)
        returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | Search ngrams in documents, ranking them by TF-IDF. We narrow our
-- search only to map/candidate terms.
searchInCorpusWithNgrams :: HasDBid NodeType
               => CorpusId
               -> ListId
               -> IsTrash
               -> NgramsType
               -> [[Text]]
               -> Maybe Offset
               -> Maybe Limit
               -> Maybe OrderBy
               -> Cmd err [FacetDoc]
searchInCorpusWithNgrams _cId _lId _t _ngt _q _o _l _order = undefined

-- | Compute TF-IDF for all 'ngramIds' in given 'CorpusId'. In this
-- case only the "TF" part makes sense and so we only compute the
-- ratio of "number of times our terms appear in given document" and
-- "number of all terms in document" and return a sorted list of
-- document ids
tfidfAll :: CorpusId -> [Int] -> Cmd err [Int]
tfidfAll cId ngramIds = do
  let ngramIdsSet = Set.fromList ngramIds
  docsWithNgrams <- runOpaQuery (queryCorpusWithNgrams cId ngramIds) :: Cmd err [(Int, Int, Int)]
  -- NOTE The query returned docs with ANY ngramIds. We need to further
  -- restrict to ALL ngramIds.
  let docsNgramsM =
        Map.fromListWith (Set.union)
            [ (ctxId, Set.singleton ngrams_id)
            | (ctxId, ngrams_id, _) <- docsWithNgrams]
  let docsWithAllNgramsS = Set.fromList $ List.map fst $
        List.filter (\(_, docNgrams) ->
                        ngramIdsSet == Set.intersection ngramIdsSet docNgrams) $ Map.toList docsNgramsM
  let docsWithAllNgrams =
        List.filter (\(ctxId, _, _) ->
                       Set.member ctxId docsWithAllNgramsS) docsWithNgrams
  -- printDebug "[tfidfAll] docsWithAllNgrams" docsWithAllNgrams
  let docsWithCounts = Map.fromListWith (+) [ (ctxId, doc_count)
                                            | (ctxId, _, doc_count) <- docsWithAllNgrams]
  -- printDebug "[tfidfAll] docsWithCounts" docsWithCounts
  let totals = [ ( ctxId
                 , ngrams_id
                 , fromIntegral doc_count :: Double
                 , fromIntegral (fromMaybe 0 $ Map.lookup ctxId docsWithCounts) :: Double)
               | (ctxId, ngrams_id, doc_count) <- docsWithAllNgrams]
  let tfidf_sorted = List.sortOn snd [(ctxId, doc_count/s)
                                     | (ctxId, _, doc_count, s) <- totals]
  pure $ List.map fst $ List.reverse tfidf_sorted

-- | Query for searching the 'context_node_ngrams' table so that we
-- find docs with ANY given 'ngramIds'.
queryCorpusWithNgrams :: CorpusId -> [Int] -> Select (Column SqlInt4, Column SqlInt4, Column SqlInt4)
queryCorpusWithNgrams cId ngramIds = proc () -> do
  row <- queryContextNodeNgramsTable -< ()
  restrict -< (_cnng_node_id row) .== (pgNodeId cId)
  restrict -< in_ (sqlInt4 <$> ngramIds) (_cnng_ngrams_id row)
  returnA -< ( _cnng_context_id row
             , _cnng_ngrams_id row
             , _cnng_doc_count row)
  --returnA -< row
  -- returnA -< ( _cnng_context_id row
  --            , _cnng_node_id row
  --            , _cnng_ngrams_id row
  --            , _cnng_ngramsType row
  --            , _cnng_weight row
  --            , _cnng_doc_count row)


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
              -> O.Select FacetDocRead
queryInCorpus cId t q = proc () -> do
  c <- queryContextSearchTable -< ()
  nc <- optionalRestrict queryNodeContextTable -<
    \nc' -> (nc' ^. nc_context_id) .== _cs_id c
  restrict -< (view nc_node_id <$> nc) .=== justFields (pgNodeId cId)
  restrict -< if t
                 then (view nc_category <$> nc) .=== justFields (sqlInt4 0)
                 else matchMaybe (view nc_category <$> nc) $ \case
                        Nothing -> toFields False
                        Just c' -> c' .>= sqlInt4 1
  restrict -< (c ^. cs_search)           @@ sqlTSQuery (unpack q)
  restrict -< (c ^. cs_typename )       .== sqlInt4 (toDBid NodeDocument)
  returnA  -< FacetDoc { facetDoc_id         = c^.cs_id
                       , facetDoc_created    = c^.cs_date
                       , facetDoc_title      = c^.cs_name
                       , facetDoc_hyperdata  = c^.cs_hyperdata
                       , facetDoc_category   = maybeFieldsToNullable (view nc_category <$> nc)
                       , facetDoc_ngramCount = maybeFieldsToNullable (view nc_score <$> nc)
                       , facetDoc_score      = maybeFieldsToNullable (view nc_score <$> nc)
                       }

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
              $ orderBy (desc _fp_score)
              $ selectGroup cId aId
              $ intercalate " | "
              $ map stemIt q

selectGroup :: HasDBid NodeType
            => CorpusId
            -> AnnuaireId
            -> Text
            -> Select FacetPairedRead
selectGroup cId aId q = proc () -> do
  (a, b, c, d) <- aggregate (p4 (groupBy, groupBy, groupBy, O.sum))
                            (selectContactViaDoc cId aId q) -< ()
  returnA -< FacetPaired a b c d


selectContactViaDoc
  :: HasDBid NodeType
  => CorpusId
  -> AnnuaireId
  -> Text
  -> SelectArr ()
               ( Field SqlInt4
               , Field SqlTimestamptz
               , Field SqlJsonb
               , Field SqlInt4
               )
selectContactViaDoc cId aId query = proc () -> do
  --(doc, (corpus, (_nodeContext_nodeContext, (annuaire, contact)))) <- queryContactViaDoc -< ()
  (contact, annuaire, _, corpus, doc) <- queryContactViaDoc -< ()
  restrict -< matchMaybe (view cs_search <$> doc) $ \case
    Nothing -> toFields False
    Just s  -> s @@ sqlTSQuery (unpack query)
  restrict -< (view cs_typename <$> doc)          .=== justFields (sqlInt4 (toDBid NodeDocument))
  restrict -< (view nc_node_id <$> corpus)        .=== justFields (pgNodeId cId)
  restrict -< (view nc_node_id <$> annuaire)      .=== justFields (pgNodeId aId)
  restrict -< (contact ^. context_typename) .== sqlInt4 (toDBid NodeContact)
  returnA  -< ( contact ^. context_id
              , contact ^. context_date
              , contact ^. context_hyperdata
              , sqlInt4 1
              )

queryContactViaDoc :: O.Select ( ContextRead
                               , MaybeFields NodeContextRead
                               , MaybeFields NodeContext_NodeContextRead
                               , MaybeFields NodeContextRead
                               , MaybeFields ContextSearchRead )
queryContactViaDoc = proc () -> do
  contact <- queryContextTable -< ()
  annuaire <- optionalRestrict queryNodeContextTable -<
    \annuaire' -> (annuaire' ^. nc_context_id) .== (contact ^. context_id)
  nodeContext_nodeContext <- optionalRestrict queryNodeContext_NodeContextTable -<
    \ncnc' -> justFields (ncnc' ^. ncnc_nodecontext2) .=== (view nc_id <$> annuaire)
  corpus <- optionalRestrict queryNodeContextTable -<
    \corpus' -> justFields (corpus' ^. nc_id) .=== (view ncnc_nodecontext1 <$> nodeContext_nodeContext)
  doc <- optionalRestrict queryContextSearchTable -<
    \doc' -> justFields (doc' ^. cs_id) .=== (view nc_context_id <$> corpus)

  returnA -< (contact, annuaire, nodeContext_nodeContext, corpus, doc)
