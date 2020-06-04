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

module Gargantext.Database.Query.Table.NodeNode
  ( module Gargantext.Database.Schema.NodeNode
  , queryNodeNodeTable
  , selectDocsDates
  , selectDocNodes
  , selectDocs
  , nodeNodesCategory
  , getNodeNode
  , insertNodeNode
  )
  where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Data.Maybe (catMaybes)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Core.Types
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Node (CorpusId, DocId)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import qualified Opaleye as O


queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = queryTable nodeNodeTable

-- | not optimized (get all ngrams without filters)
_nodesNodes :: Cmd err [NodeNode]
_nodesNodes = runOpaQuery queryNodeNodeTable

------------------------------------------------------------------------
-- | Basic NodeNode tools
getNodeNode :: NodeId -> Cmd err [NodeNode]
getNodeNode n = runOpaQuery (selectNodeNode $ pgNodeId n)
  where
    selectNodeNode :: Column PGInt4 -> Query NodeNodeRead
    selectNodeNode n' = proc () -> do
      ns <- queryNodeNodeTable -< ()
      restrict -< _nn_node1_id ns .== n'
      returnA -< ns

------------------------------------------------------------------------
insertNodeNode :: [NodeNode] -> Cmd err Int64
insertNodeNode ns = mkCmd $ \conn -> runInsert_ conn
                          $ Insert nodeNodeTable ns' rCount Nothing
  where
    ns' :: [NodeNodeWrite]
    ns' = map (\(NodeNode n1 n2 x y)
                -> NodeNode (pgNodeId n1)
                            (pgNodeId n2)
                            (pgDouble <$> x)
                            (pgInt4   <$> y)
              ) ns


-- | Favorite management
_nodeNodeCategory :: CorpusId -> DocId -> Int -> Cmd err [Int]
_nodeNodeCategory cId dId c = map (\(PGS.Only a) -> a) <$> runPGSQuery favQuery (c,cId,dId)
  where
    favQuery :: PGS.Query
    favQuery = [sql|UPDATE nodes_nodes SET category = ?
               WHERE node1_id = ? AND node2_id = ?
               RETURNING node2_id;
               |]

nodeNodesCategory :: [(CorpusId,DocId,Int)] -> Cmd err [Int]
nodeNodesCategory inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery catQuery (PGS.Only $ Values fields inputData)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","int4"]
    catQuery :: PGS.Query
    catQuery = [sql| UPDATE nodes_nodes as nn0
                      SET category = nn1.category
                       FROM (?) as nn1(node1_id,node2_id,category)
                       WHERE nn0.node1_id = nn1.node1_id
                       AND   nn0.node2_id = nn1.node2_id
                       RETURNING nn1.node2_id
                  |]

------------------------------------------------------------------------
-- | TODO use UTCTime fast 
selectDocsDates :: CorpusId -> Cmd err [Text]
selectDocsDates cId =  map (head' "selectDocsDates" . splitOn "-")
                   <$> catMaybes
                   <$> map (view hyperdataDocument_publication_date)
                   <$> selectDocs cId

selectDocs :: CorpusId -> Cmd err [HyperdataDocument]
selectDocs cId = runOpaQuery (queryDocs cId)

queryDocs :: CorpusId -> O.Query (Column PGJsonb)
queryDocs cId = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< nn^.nn_node1_id  .== (toNullable $ pgNodeId cId)
  restrict -< nn^.nn_category  .>= (toNullable $ pgInt4 1)
  restrict -< n^.node_typename .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA -< view (node_hyperdata) n

selectDocNodes :: CorpusId -> Cmd err [Node HyperdataDocument]
selectDocNodes cId = runOpaQuery (queryDocNodes cId)

queryDocNodes :: CorpusId -> O.Query NodeRead
queryDocNodes cId = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< nn^.nn_node1_id  .== (toNullable $ pgNodeId cId)
  restrict -< nn^.nn_category  .>= (toNullable $ pgInt4 1)
  restrict -< n^.node_typename .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA -<  n

joinInCorpus :: O.Query (NodeRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeTable queryNodeNodeTable cond
  where
    cond :: (NodeRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nn^.nn_node2_id .== (view node_id n)

