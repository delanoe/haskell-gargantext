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

import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Gargantext.Database.Utils
import Gargantext.Database.Types.Node (CorpusId, DocId)
import Gargantext.Prelude
import Opaleye


data NodeNodePoly node1_id node2_id score fav del
                   = NodeNode { nodeNode_node1_id   :: node1_id
                              , nodeNode_node2_id   :: node2_id
                              , nodeNode_score :: score
                              , nodeNode_favorite :: fav
                              , nodeNode_delete   :: del
                              } deriving (Show)

type NodeNodeWrite     = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGFloat8)))
                                      (Maybe  (Column (PGBool)))
                                      (Maybe  (Column (PGBool)))

type NodeNodeRead      = NodeNodePoly (Column (PGInt4))
                                      (Column (PGInt4))
                                      (Column (PGFloat8))
                                      (Column (PGBool))
                                      (Column (PGBool))
                                      
type NodeNodeReadNull  = NodeNodePoly (Column (Nullable PGInt4))
                                      (Column (Nullable PGInt4))
                                      (Column (Nullable PGFloat8))
                                      (Column (Nullable PGBool))
                                      (Column (Nullable PGBool))

type NodeNode = NodeNodePoly Int Int (Maybe Double) (Maybe Bool) (Maybe Bool)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
$(makeLensesWith abbreviatedFields   ''NodeNodePoly)

nodeNodeTable :: Table NodeNodeWrite NodeNodeRead
nodeNodeTable  = Table "nodes_nodes" (pNodeNode
                                NodeNode { nodeNode_node1_id = required "node1_id"
                                         , nodeNode_node2_id = required "node2_id"
                                         , nodeNode_score    = optional "score"
                                         , nodeNode_favorite = optional "favorite"
                                         , nodeNode_delete   = optional "delete"
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

instance QueryRunnerColumnDefault PGBool (Maybe Bool) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


------------------------------------------------------------------------
-- | Favorite management
nodeToFavorite :: CorpusId -> DocId -> Bool -> Cmd err [Int]
nodeToFavorite cId dId b = map (\(PGS.Only a) -> a) <$> runPGSQuery favQuery (b,cId,dId)
  where
    favQuery :: PGS.Query
    favQuery = [sql|UPDATE nodes_nodes SET favorite = ?
               WHERE node1_id = ? AND node2_id = ?
               RETURNING node2_id;
               |]

nodesToFavorite :: [(CorpusId,DocId,Bool)] -> Cmd err [Int]
nodesToFavorite inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery trashQuery (PGS.Only $ Values fields inputData)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","bool"]
    trashQuery :: PGS.Query
    trashQuery = [sql| UPDATE nodes_nodes as old SET
                 favorite = new.favorite
                 from (?) as new(node1_id,node2_id,favorite)
                 WHERE old.node1_id = new.node1_id
                 AND   old.node2_id = new.node2_id
                 RETURNING new.node2_id
                  |]

------------------------------------------------------------------------
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


