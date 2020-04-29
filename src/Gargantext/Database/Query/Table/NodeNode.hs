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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeNode where

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
import Gargantext.Database.Admin.Utils
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import qualified Opaleye as O

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

-------------------------
insertNodeNode :: [NodeNode] -> Cmd err Int64
insertNodeNode ns = mkCmd $ \conn -> runInsert_ conn $ Insert nodeNodeTable ns' rCount Nothing
  where
    ns' :: [NodeNodeWrite]
    ns' = map (\(NodeNode n1 n2 x y)
                -> NodeNode (pgNodeId n1)
                            (pgNodeId n2)
                            (pgDouble <$> x)
                            (pgInt4   <$> y)
              ) ns


-- | Favorite management
nodeNodeCategory :: CorpusId -> DocId -> Int -> Cmd err [Int]
nodeNodeCategory cId dId c = map (\(PGS.Only a) -> a) <$> runPGSQuery favQuery (c,cId,dId)
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

------------------------------------------------------------------------
-- | Trash management
nodeToTrash :: CorpusId -> DocId -> Bool -> Cmd err [PGS.Only Int]
nodeToTrash cId dId b = runPGSQuery trashQuery (b,cId,dId)
  where
    trashQuery :: PGS.Query
    trashQuery = [sql|UPDATE nodes_nodes SET delete = ?
                  WHERE node1_id = ? AND node2_id = ?
                  RETURNING node2_id
                  |]

-- | Trash Massive
nodesToTrash :: [(CorpusId,DocId,Bool)] -> Cmd err [Int]
nodesToTrash input = map (\(PGS.Only a) -> a)
                        <$> runPGSQuery trashQuery (PGS.Only $ Values fields input)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","bool"]
    trashQuery :: PGS.Query
    trashQuery = [sql| UPDATE nodes_nodes as nn0 SET
                 delete = nn1.delete
                 from (?) as nn1(node1_id,node2_id,delete)
                 WHERE nn0.node1_id = nn1.node1_id
                 AND   nn0.node2_id = nn1.node2_id
                 RETURNING nn1.node2_id
                  |]

-- | /!\ Really remove nodes in the Corpus or Annuaire
emptyTrash :: CorpusId -> Cmd err [PGS.Only Int]
emptyTrash cId = runPGSQuery delQuery (PGS.Only cId)
  where
    delQuery :: PGS.Query
    delQuery = [sql|DELETE from nodes_nodes n
                    WHERE n.node1_id = ?
                      AND n.delete = true
                    RETURNING n.node2_id
                |]
------------------------------------------------------------------------
