{-|
Module      : Gargantext.Database.Schema.NodeNode
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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNode where

import Control.Lens (view)
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe, catMaybes)
import Data.Text (Text, splitOn)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Gargantext.Database.Schema.Node 
import Gargantext.Core.Types
import Gargantext.Database.Utils
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node (CorpusId, DocId)
import Gargantext.Prelude
import Opaleye
import Control.Arrow (returnA)
import qualified Opaleye as O

data NodeNodePoly node1_id node2_id score cat
                   = NodeNode { nn_node1_id   :: node1_id
                              , nn_node2_id   :: node2_id
                              , nn_score      :: score
                              , nn_category   :: cat
                              } deriving (Show)

type NodeNodeWrite     = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGFloat8)))
                                      (Maybe  (Column (PGInt4)))

type NodeNodeRead      = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Column (PGFloat8))
                                      (Column (PGInt4))
                                      
type NodeNodeReadNull  = NodeNodePoly (Column (Nullable PGInt4))
                                      (Column (Nullable PGInt4))
                                      (Column (Nullable PGFloat8))
                                      (Column (Nullable PGInt4))

type NodeNode = NodeNodePoly Int Int (Maybe Double) (Maybe Int)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
$(makeLensesWith abbreviatedFields   ''NodeNodePoly)

nodeNodeTable :: Table NodeNodeWrite NodeNodeRead
nodeNodeTable  = Table "nodes_nodes" (pNodeNode
                                NodeNode { nn_node1_id = required "node1_id"
                                         , nn_node2_id = required "node2_id"
                                         , nn_score    = optional "score"
                                         , nn_category = optional "category"
                                     }
                                     )

queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = queryTable nodeNodeTable


-- | not optimized (get all ngrams without filters)
nodesNodes :: Cmd err [NodeNode]
nodesNodes = runOpaQuery queryNodeNodeTable

instance QueryRunnerColumnDefault (Nullable PGInt4) Int where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


------------------------------------------------------------------------
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
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","category"]
    catQuery :: PGS.Query
    catQuery = [sql| UPDATE nodes_nodes as old SET
                 category = new.category
                 from (?) as new(node1_id,node2_id,category)
                 WHERE old.node1_id = new.node1_id
                 AND   old.node2_id = new.node2_id
                 RETURNING new.node2_id
                  |]

------------------------------------------------------------------------
-- | TODO use UTCTime fast 
selectDocsDates :: CorpusId -> Cmd err [Text]
selectDocsDates cId = 
                map (head' "selectDocsDates" . splitOn "-")
               <$> catMaybes
               <$> map (view hyperdataDocument_publication_date)
               <$> selectDocs cId


selectDocs :: CorpusId -> Cmd err [HyperdataDocument]
selectDocs cId = runOpaQuery (queryDocs cId)

queryDocs :: CorpusId -> O.Query (Column PGJsonb)
queryDocs cId = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< ( nn_node1_id nn)  .== (toNullable $ pgNodeId cId)
  restrict -< ( nn_category nn)  .>= (toNullable $ pgInt4 1)
  restrict -< (_node_typename n) .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA -< view (node_hyperdata) n


selectDocNodes :: CorpusId -> Cmd err [NodeDocument]
selectDocNodes cId = runOpaQuery (queryDocNodes cId)

queryDocNodes :: CorpusId -> O.Query NodeRead
queryDocNodes cId = proc () -> do
  (n, nn) <- joinInCorpus -< ()
  restrict -< ( nn_node1_id nn)  .== (toNullable $ pgNodeId cId)
  restrict -< ( nn_category nn)  .>= (toNullable $ pgInt4 1)
  restrict -< (_node_typename n) .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA -<  n



joinInCorpus :: O.Query (NodeRead, NodeNodeReadNull)
joinInCorpus = leftJoin queryNodeTable queryNodeNodeTable cond
  where
    cond :: (NodeRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nn_node2_id nn .== (view node_id n)


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
    trashQuery = [sql| UPDATE nodes_nodes as old SET
                 delete = new.delete
                 from (?) as new(node1_id,node2_id,delete)
                 WHERE old.node1_id = new.node1_id
                 AND   old.node2_id = new.node2_id
                 RETURNING new.node2_id
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
