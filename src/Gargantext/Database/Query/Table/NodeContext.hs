{-|
Module      : Gargantext.Database.Query.Table.NodeNode
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeContext
  ( module Gargantext.Database.Schema.NodeContext
  , queryNodeContextTable
  , selectDocsDates
  , selectDocNodes
  , selectDocs
  , nodeContextsCategory
  , nodeContextsScore
  , getNodeContext
  , insertNodeContext
  , deleteNodeContext
  , selectPublicContexts
  , selectCountDocs
  )
  where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Data.Maybe (catMaybes)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import qualified Opaleye as O

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeContext
import Gargantext.Prelude

queryNodeContextTable :: Select NodeContextRead
queryNodeContextTable = selectTable nodeContextTable

-- | not optimized (get all ngrams without filters)
_nodesContexts :: Cmd err [NodeContext]
_nodesContexts = runOpaQuery queryNodeContextTable

------------------------------------------------------------------------
-- | Basic NodeContext tools
getNodeContext :: NodeId -> Cmd err [NodeContext]
getNodeContext n = runOpaQuery (selectNodeContext $ pgNodeId n)
  where
    selectNodeContext :: Column SqlInt4 -> Select NodeContextRead
    selectNodeContext n' = proc () -> do
      ns <- queryNodeContextTable -< ()
      restrict -< _nc_node_id ns .== n'
      returnA -< ns

------------------------------------------------------------------------
insertNodeContext :: [NodeContext] -> Cmd err Int
insertNodeContext ns = mkCmd $ \conn -> fromIntegral <$> (runInsert_ conn
                          $ Insert nodeContextTable ns' rCount (Just DoNothing))
  where
    ns' :: [NodeContextWrite]
    ns' = map (\(NodeContext n c x y)
                -> NodeContext (pgNodeId n)
                               (pgNodeId c)
                               (sqlDouble <$> x)
                               (sqlInt4  <$> y)
              ) ns


------------------------------------------------------------------------
type Node_Id    = NodeId
type Context_Id = NodeId

deleteNodeContext :: Node_Id -> Context_Id -> Cmd err Int
deleteNodeContext n c = mkCmd $ \conn ->
  fromIntegral <$> runDelete_ conn
                              (Delete nodeContextTable
                                      (\(NodeContext n_id c_id _ _) -> n_id .== pgNodeId n
                                                                   .&& c_id .== pgNodeId c
                                      )
                                      rCount
                              )

------------------------------------------------------------------------
-- | Favorite management
nodeContextsCategory :: [(CorpusId, DocId, Int)] -> Cmd err [Int]
nodeContextsCategory inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery catSelect (PGS.Only $ Values fields inputData)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","int4"]
    catSelect :: PGS.Query
    catSelect = [sql| UPDATE nodes_contexts as nn0
                      SET category = nn1.category
                       FROM (?) as nn1(node_id,context_id,category)
                       WHERE nn0.node_id    = nn1.node_id
                       AND   nn0.context_id = nn1.context_id
                       RETURNING nn1.node_id
                  |]

------------------------------------------------------------------------
-- | Score management
nodeContextsScore :: [(CorpusId, DocId, Int)] -> Cmd err [Int]
nodeContextsScore inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery catScore (PGS.Only $ Values fields inputData)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","int4"]
    catScore :: PGS.Query
    catScore = [sql| UPDATE nodes_contexts as nn0
                      SET score = nn1.score
                       FROM (?) as nn1(node_id, context_id, score)
                       WHERE nn0.node_id    = nn1.node_id
                       AND   nn0.context_id = nn1.context_id
                       RETURNING nn1.context_id
                  |]


------------------------------------------------------------------------
selectCountDocs :: HasDBid NodeType => CorpusId -> Cmd err Int
selectCountDocs cId = runCountOpaQuery (queryCountDocs cId)
  where
    queryCountDocs cId' = proc () -> do
      (c, nc) <- joinInCorpus -< ()
      restrict -< nc^.nc_node_id   .== (toNullable $ pgNodeId cId')
      restrict -< nc^.nc_category  .>= (toNullable $ sqlInt4 1)
      restrict -< c^.context_typename .== (sqlInt4 $ toDBid NodeDocument)
      returnA -< c


-- | TODO use UTCTime fast
selectDocsDates :: HasDBid NodeType => CorpusId -> Cmd err [Text]
selectDocsDates cId =  map (head' "selectDocsDates" . splitOn "-")
                   <$> catMaybes
                   <$> map (view hd_publication_date)
                   <$> selectDocs cId

selectDocs :: HasDBid NodeType => CorpusId -> Cmd err [HyperdataDocument]
selectDocs cId = runOpaQuery (queryDocs cId)

queryDocs :: HasDBid NodeType => CorpusId -> O.Select (Column SqlJsonb)
queryDocs cId = proc () -> do
  (c, nn)  <- joinInCorpus -< ()
  restrict -< nn^.nc_node_id      .== (toNullable $ pgNodeId cId)
  restrict -< nn^.nc_category     .>= (toNullable $ sqlInt4 1)
  restrict -< c^.context_typename .== (sqlInt4 $ toDBid NodeDocument)
  returnA  -< view (context_hyperdata) c

selectDocNodes :: HasDBid NodeType => CorpusId -> Cmd err [Context HyperdataDocument]
selectDocNodes cId = runOpaQuery (queryDocNodes cId)

queryDocNodes :: HasDBid NodeType => CorpusId -> O.Select ContextRead
queryDocNodes cId = proc () -> do
  (c, nc) <- joinInCorpus -< ()
  restrict -< nc^.nc_node_id   .== (toNullable $ pgNodeId cId)
  restrict -< nc^.nc_category  .>= (toNullable $ sqlInt4 1)
  restrict -< c^.context_typename .== (sqlInt4 $ toDBid NodeDocument)
  returnA -<  c

joinInCorpus :: O.Select (ContextRead, NodeContextReadNull)
joinInCorpus = leftJoin queryContextTable queryNodeContextTable cond
  where
    cond :: (ContextRead, NodeContextRead) -> Column SqlBool
    cond (c, nc) = c^.context_id .== nc^.nc_node_id


joinOn1 :: O.Select (NodeRead, NodeContextReadNull)
joinOn1 = leftJoin queryNodeTable queryNodeContextTable cond
  where
    cond :: (NodeRead, NodeContextRead) -> Column SqlBool
    cond (n, nc) = nc^.nc_node_id .== n^.node_id


------------------------------------------------------------------------
selectPublicContexts :: HasDBid NodeType => (Hyperdata a, DefaultFromField SqlJsonb a)
                  => Cmd err [(Node a, Maybe Int)]
selectPublicContexts = runOpaQuery (queryWithType NodeFolderPublic)

queryWithType :: HasDBid NodeType =>NodeType -> O.Select (NodeRead, Column (Nullable SqlInt4))
queryWithType nt = proc () -> do
  (n, nc) <- joinOn1 -< ()
  restrict -< n^.node_typename .== (sqlInt4 $ toDBid nt)
  returnA  -<  (n, nc^.nc_context_id)
