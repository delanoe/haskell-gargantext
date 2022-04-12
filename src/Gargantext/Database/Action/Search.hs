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
import Gargantext.Database.Query.Table.NodeContext
import Gargantext.Database.Query.Table.NodeContext_NodeContext
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
              $ orderBy (desc _fp_score)
              $ selectGroup cId aId
              $ intercalate " | "
              $ map stemIt q

selectGroup :: HasDBid NodeType
            => CorpusId
            -> AnnuaireId
            -> Text
            -> Select FacetPairedReadNull
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
               ( Column (Nullable SqlInt4)
               , Column (Nullable SqlTimestamptz)
               , Column (Nullable SqlJsonb)
               , Column (Nullable SqlInt4)
               )
selectContactViaDoc cId aId query = proc () -> do
  (doc, (corpus, (_nodeContext_nodeContext, (annuaire, contact)))) <- queryContactViaDoc -< ()
  restrict -< (doc^.cs_search)             @@ (sqlTSQuery $ unpack query                )
  restrict -< (doc^.cs_typename)          .== (sqlInt4    $ toDBid NodeDocument         )
  restrict -< (corpus^.nc_node_id)        .== (toNullable $ pgNodeId cId                )
  restrict -< (annuaire^.nc_node_id)      .== (toNullable $ pgNodeId aId                )
  restrict -< (contact^.context_typename) .== (toNullable $ sqlInt4 $ toDBid NodeContact)
  returnA  -< ( contact^.context_id
              , contact^.context_date
              , contact^.context_hyperdata
              , toNullable $ sqlInt4 1
              )

queryContactViaDoc :: O.Select ( ContextSearchRead
                               , ( NodeContextReadNull
                                 , ( NodeContext_NodeContextReadNull
                                   , ( NodeContextReadNull
                                     , ContextReadNull
                                     )
                                   )
                                 )
                               )
queryContactViaDoc =
  leftJoin5
  queryContextTable
  queryNodeContextTable
  queryNodeContext_NodeContextTable
  queryNodeContextTable
  queryContextSearchTable
  cond12
  cond23
  cond34
  cond45
    where
      cond12 :: (NodeContextRead, ContextRead) -> Column SqlBool
      cond12 (annuaire, contact) = contact^.context_id .== annuaire^.nc_context_id

      cond23 :: ( NodeContext_NodeContextRead
                , ( NodeContextRead
                  , ContextReadNull
                  )
                ) -> Column SqlBool
      cond23 (nodeContext_nodeContext, (annuaire, _)) = nodeContext_nodeContext^.ncnc_nodecontext2 .== annuaire^.nc_id

      cond34 :: ( NodeContextRead
                , ( NodeContext_NodeContextRead
                  , ( NodeContextReadNull
                    , ContextReadNull
                    )
                  )
                ) -> Column SqlBool
      cond34 (corpus, (nodeContext_nodeContext, (_,_))) =  nodeContext_nodeContext^.ncnc_nodecontext1 .== corpus^.nc_id


      cond45 :: ( ContextSearchRead
                , ( NodeContextRead
                  , ( NodeContext_NodeContextReadNull
                    , ( NodeContextReadNull
                      , ContextReadNull
                      )
                    )
                  )
                ) -> Column SqlBool
      cond45 (doc, (corpus, (_,(_,_)))) = doc^.cs_id .== corpus^.nc_context_id

