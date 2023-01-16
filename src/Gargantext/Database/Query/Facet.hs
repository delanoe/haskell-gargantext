{-|
Module      : Gargantext.Database.Query.Facet
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
------------------------------------------------------------------------
module Gargantext.Database.Query.Facet
  ( runViewAuthorsDoc
  , runViewDocuments
--   , viewDocuments'
  , runCountDocuments
  , filterWith

  , Category
  , Score
  , Title

  , Pair(..)
  , Facet(..)
  , FacetDoc
  , FacetDocRead
  , FacetPaired(..)
  , FacetPairedRead
  , FacetPairedReadNull
  , FacetPairedReadNullAgg
  , OrderBy(..)
  )
  where

import Control.Arrow (returnA, (>>>))
import Control.Lens ((^.))
import qualified Data.Text as T
import Opaleye
import Protolude hiding (null, map, sum, not)
import qualified Opaleye.Internal.Unpackspec()

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Query.Filter
import Gargantext.Database.Query.Join (leftJoin5)
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.Context
import Gargantext.Database.Query.Facet.Types
import Gargantext.Database.Query.Table.ContextNodeNgrams
import Gargantext.Database.Query.Table.NodeContext (queryNodeContextTable)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeContext
import Gargantext.Prelude (printDebug)

------------------------------------------------------------------------


-- TODO-SECURITY check
runViewAuthorsDoc :: HasDBid NodeType
                  => ContactId
                  -> IsTrash
                  -> Maybe Offset
                  -> Maybe Limit
                  -> Maybe OrderBy
                  -> Cmd err [FacetDoc]
runViewAuthorsDoc cId t o l order = runOpaQuery $ filterWith o l order $ viewAuthorsDoc cId t ntId
  where
    ntId = NodeDocument

-- TODO add delete ?
viewAuthorsDoc :: HasDBid NodeType
               => ContactId
               -> IsTrash
               -> NodeType
               -> Select FacetDocRead
viewAuthorsDoc cId _ nt = proc () -> do
  (doc,(_,(_,(_,contact')))) <- queryAuthorsDoc      -< ()

  restrict -< _node_id   contact'  .== (toNullable $ pgNodeId cId)
  restrict -< _node_typename doc   .== (sqlInt4 $ toDBid nt)

  returnA  -< FacetDoc { facetDoc_id         = _node_id        doc
                       , facetDoc_created    = _node_date      doc
                       , facetDoc_title      = _node_name      doc
                       , facetDoc_hyperdata  = _node_hyperdata doc
                       , facetDoc_category   = toNullable $ sqlInt4 1
                       , facetDoc_ngramCount = toNullable $ sqlDouble 1
                       , facetDoc_score      = toNullable $ sqlDouble 1 }

queryAuthorsDoc :: Select (NodeRead, (ContextNodeNgramsReadNull, (NgramsReadNull, (ContextNodeNgramsReadNull, NodeReadNull))))
queryAuthorsDoc = leftJoin5 queryNodeTable queryContextNodeNgramsTable queryNgramsTable queryContextNodeNgramsTable queryNodeTable cond12 cond23 cond34 cond45
    where
         cond12 :: (ContextNodeNgramsRead, NodeRead) -> Column SqlBool
         cond12 (nodeNgram, doc) =  _node_id doc
                                .== _cnng_context_id nodeNgram

         cond23 :: (NgramsRead, (ContextNodeNgramsRead, NodeReadNull)) -> Column SqlBool
         cond23 (ngrams', (nodeNgram, _)) =  ngrams'^.ngrams_id
                                        .== _cnng_ngrams_id nodeNgram

         cond34 :: (ContextNodeNgramsRead, (NgramsRead, (ContextNodeNgramsReadNull, NodeReadNull))) -> Column SqlBool
         cond34 (nodeNgram2, (ngrams', (_,_)))= ngrams'^.ngrams_id .== _cnng_ngrams_id       nodeNgram2

         cond45 :: (NodeRead, (ContextNodeNgramsRead, (NgramsReadNull, (ContextNodeNgramsReadNull, NodeReadNull)))) -> Column SqlBool
         cond45 (contact', (nodeNgram2', (_, (_,_)))) = _node_id  contact'  .== _cnng_context_id         nodeNgram2'


------------------------------------------------------------------------
-- TODO-SECURITY check
runViewDocuments :: HasDBid NodeType
                 => CorpusId
                 -> IsTrash
                 -> Maybe Offset
                 -> Maybe Limit
                 -> Maybe OrderBy
                 -> Maybe Text
                 -> Maybe Text
                 -> Cmd err [FacetDoc]
runViewDocuments cId t o l order query year = do
    printDebug "[runViewDocuments] sqlQuery" $ showSql sqlQuery
    runOpaQuery $ filterWith o l order sqlQuery
  where
    sqlQuery = viewDocuments cId t (toDBid NodeDocument) query year

runCountDocuments :: HasDBid NodeType => CorpusId -> IsTrash -> Maybe Text -> Maybe Text -> Cmd err Int
runCountDocuments cId t mQuery mYear = do
  runCountOpaQuery sqlQuery
  where
    sqlQuery = viewDocuments cId t (toDBid NodeDocument) mQuery mYear


viewDocuments :: CorpusId
              -> IsTrash
              -> NodeTypeId
              -> Maybe Text
              -> Maybe Text
              -> Select FacetDocRead
viewDocuments cId t ntId mQuery mYear = viewDocumentsQuery cId t ntId mQuery mYear >>> proc (c, nc) -> do
  returnA  -< FacetDoc { facetDoc_id         = _cs_id        c
                       , facetDoc_created    = _cs_date      c
                       , facetDoc_title      = _cs_name      c
                       , facetDoc_hyperdata  = _cs_hyperdata c
                       , facetDoc_category   = toNullable $ nc^.nc_category
                       , facetDoc_ngramCount = toNullable $ nc^.nc_score
                       , facetDoc_score      = toNullable $ nc^.nc_score
                       }

viewDocumentsQuery :: CorpusId
                   -> IsTrash
                   -> NodeTypeId
                   -> Maybe Text
                   -> Maybe Text
                   -> Select (ContextSearchRead, NodeContextRead)
viewDocumentsQuery cId t ntId mQuery mYear = proc () -> do
  c  <- queryContextSearchTable -< ()
  nc <- queryNodeContextTable   -< ()
  restrict -< c^.cs_id         .== nc^.nc_context_id
  restrict -< nc^.nc_node_id   .== (pgNodeId cId)
  restrict -< c^.cs_typename   .== (sqlInt4 ntId)
  restrict -< if t then nc^.nc_category .== (sqlInt4 0)
                   else nc^.nc_category .>= (sqlInt4 1)

  let
    query         = (fromMaybe "" mQuery)
    year          = (fromMaybe "" mYear)
    iLikeQuery    = T.intercalate "" ["%", query, "%"]
    abstractLHS h = fromNullable (sqlStrictText "")
                  $ toNullable h .->> (sqlStrictText "abstract")
    yearLHS h     = fromNullable (sqlStrictText "")
                  $ toNullable h .->> (sqlStrictText "publication_year")

  restrict -<
    if query == "" then sqlBool True
      else  ((c^.cs_name) `ilike` (sqlStrictText iLikeQuery))
        .|| ((abstractLHS (c^.cs_hyperdata)) `ilike` (sqlStrictText iLikeQuery))
  restrict -<
    if year == "" then sqlBool True
      else (yearLHS (c^.cs_hyperdata)) .== (sqlStrictText year)

  returnA -< (c, nc)


------------------------------------------------------------------------
filterWith :: (SqlOrd date, SqlOrd title, SqlOrd category, SqlOrd score, hyperdata ~ Column SqlJsonb) =>
        Maybe Gargantext.Core.Types.Offset
     -> Maybe Gargantext.Core.Types.Limit
     -> Maybe OrderBy
     -> Select (Facet id (Column date) (Column title) hyperdata (Column category) ngramCount (Column score))
     -> Select (Facet id (Column date) (Column title) hyperdata (Column category) ngramCount (Column score))
filterWith o l order q = limit' l $ offset' o $ orderBy (orderWith order) q


orderWith :: (SqlOrd b1, SqlOrd b2, SqlOrd b3, SqlOrd b4)
          => Maybe OrderBy
          -> Order (Facet id (Column b1) (Column b2) (Column SqlJsonb) (Column b3) ngramCount (Column b4))
orderWith (Just DateAsc)   = asc  facetDoc_created
orderWith (Just DateDesc)  = desc facetDoc_created

orderWith (Just TitleAsc)  = asc  facetDoc_title
orderWith (Just TitleDesc) = desc facetDoc_title

orderWith (Just ScoreAsc)  = asc  facetDoc_score
orderWith (Just ScoreDesc) = descNullsLast facetDoc_score

orderWith (Just SourceAsc)  = asc  facetDoc_source
orderWith (Just SourceDesc) = desc facetDoc_source

orderWith (Just TagAsc)     = asc  facetDoc_category
orderWith (Just TagDesc)    = desc facetDoc_category

orderWith _                = asc facetDoc_created

facetDoc_source :: SqlIsJson a
                => Facet id created title (Column a) favorite ngramCount score
                -> Column (Nullable SqlText)
facetDoc_source x = toNullable (facetDoc_hyperdata x) .->> sqlString "source"
