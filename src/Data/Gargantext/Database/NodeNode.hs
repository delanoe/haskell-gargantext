{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Gargantext.Database.NodeNode where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye

import Data.Gargantext.Database.Private (infoGargandb)

data NodeNodePoly node1_id node2_id score
                   = NodeNode { nodeNode_node1_id   :: node1_id
                              , nodeNode_node2_id   :: node2_id
                              , nodeNode_score :: score
                              } deriving (Show)

type NodeNodeWrite = NodeNodePoly (Column PGInt4) (Column PGInt4) (Maybe (Column PGFloat8))
type NodeNodeRead  = NodeNodePoly (Column PGInt4) (Column PGInt4) (Column PGFloat8)


type NodeNode = NodeNodePoly Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
$(makeLensesWith abbreviatedFields   ''NodeNodePoly)


nodeNodeTable :: Table NodeNodeWrite NodeNodeRead
nodeNodeTable  = Table "nodes_nodes" (pNodeNode NodeNode { nodeNode_node1_id = required "node1_id"
                                                           , nodeNode_node2_id = required "node2_id"
                                                           , nodeNode_score    = optional "score"
                                                           }
                                       )


queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = queryTable nodeNodeTable


-- | not optimized (get all ngrams without filters)
nodeNodes :: IO [NodeNode]
nodeNodes = do
    conn <- PGS.connect infoGargandb
    runQuery conn queryNodeNodeTable

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
