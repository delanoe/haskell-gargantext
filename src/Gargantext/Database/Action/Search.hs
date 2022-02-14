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
import Data.Maybe
import Data.Text (Text, unpack, intercalate)
import Data.Time (UTCTime)
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..), HyperdataContact(..))
import Gargantext.Database.Prelude (Cmd, runOpaQuery, runCountOpaQuery)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Filter
import Gargantext.Database.Query.Join (leftJoin5)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Context
import Gargantext.Database.Query.Table.NodeNode
import Gargantext.Database.Query.Table.NodeContext
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
searchDocInDatabase _p t = runOpaQuery (queryDocInDatabase t)
  where
    -- | Global search query where ParentId is Master Node Corpus Id 
    queryDocInDatabase :: Text -> O.Select (Column SqlInt4, Column SqlJsonb)
    queryDocInDatabase q = proc () -> do
        row <- queryNodeSearchTable -< ()
        restrict -< (_ns_search row)    @@ (sqlTSQuery (unpack q))
        restrict -< (_ns_typename row) .== (sqlInt4 $ toDBid NodeDocument)
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
              -> O.Select FacetDocRead
queryInCorpus cId t q = proc () -> do
  (c, nc) <- joinInCorpus -< ()
  restrict -< (nc^.nc_node_id) .== (toNullable $ pgNodeId cId)
  restrict -< if t
                 then (nc^.nc_category) .== (toNullable $ sqlInt4 0)
                 else (nc^.nc_category) .>= (toNullable $ sqlInt4 1)
  restrict -< (c ^. cs_search)           @@ (sqlTSQuery (unpack q))
  restrict -< (c ^. cs_typename )       .== (sqlInt4 $ toDBid NodeDocument)
  returnA  -< FacetDoc { facetDoc_id         = c^.cs_id
                       , facetDoc_created    = c^.cs_date
                       , facetDoc_title      = c^.cs_name
                       , facetDoc_hyperdata  = c^.cs_hyperdata
                       , facetDoc_category   = nc^.nc_category
                       , facetDoc_ngramCount = nc^.nc_score
                       , facetDoc_score      = nc^.nc_score
                       }

joinInCorpus :: O.Select (ContextSearchRead, NodeContextReadNull)
joinInCorpus = leftJoin queryContextSearchTable queryNodeContextTable cond
  where
    cond :: (ContextSearchRead, NodeContextRead) -> Column SqlBool
    cond (c, nc) = nc^.nc_context_id .== _cs_id c

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
  -> SelectArr ()
               ( Column (Nullable SqlInt4)
               , Column (Nullable SqlTimestamptz)
               , Column (Nullable SqlJsonb)
               , Column (Nullable SqlInt4)
               )
selectContactViaDoc cId aId q = proc () -> do
  (doc, (corpus_doc, (_contact_doc, (annuaire_contact, contact)))) <- queryContactViaDoc -< ()
  restrict -< (doc^.ns_search)           @@ (sqlTSQuery  $ unpack q  )
  restrict -< (doc^.ns_typename)        .== (sqlInt4 $ toDBid NodeDocument)
  restrict -< (corpus_doc^.nn_node1_id)  .== (toNullable $ pgNodeId cId)
  restrict -< (annuaire_contact^.nn_node1_id) .== (toNullable $ pgNodeId aId)
  restrict -< (contact^.node_typename)        .== (toNullable $ sqlInt4 $ toDBid NodeContact)
  returnA  -< ( contact^.node_id
              , contact^.node_date
              , contact^.node_hyperdata
              , toNullable $ sqlInt4 1
              )

selectGroup :: HasDBid NodeType
            => NodeId
            -> NodeId
            -> Text
            -> Select FacetPairedReadNull
selectGroup cId aId q = proc () -> do
  (a, b, c, d) <- aggregate (p4 (groupBy, groupBy, groupBy, O.sum))
                            (selectContactViaDoc cId aId q) -< ()
  returnA -< FacetPaired a b c d


queryContactViaDoc :: O.Select ( NodeSearchRead
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
      cond12 :: (NodeNodeRead, NodeRead) -> Column SqlBool
      cond12 (annuaire_contact, contact) = contact^.node_id .== annuaire_contact^.nn_node2_id

      cond23 :: ( NodeNodeRead
                , ( NodeNodeRead
                  , NodeReadNull
                  )
                ) -> Column SqlBool
      cond23 (contact_doc, (annuaire_contact, _)) = contact_doc^.nn_node1_id .== annuaire_contact^.nn_node2_id

      cond34 :: ( NodeNodeRead
                , ( NodeNodeRead
                  , ( NodeNodeReadNull
                    , NodeReadNull
                    )
                  )
                ) -> Column SqlBool
      cond34 (corpus_doc, (contact_doc, (_,_))) =  corpus_doc^.nn_node2_id .== contact_doc^.nn_node2_id


      cond45 :: ( NodeSearchRead
                , ( NodeNodeRead
                  , ( NodeNodeReadNull
                    , ( NodeNodeReadNull
                      , NodeReadNull
                      )
                    )
                  )
                ) -> Column SqlBool
      cond45 (doc, (corpus_doc, (_,(_,_)))) = doc^.ns_id .== corpus_doc^.nn_node2_id


------------------------------------------------------------------------
