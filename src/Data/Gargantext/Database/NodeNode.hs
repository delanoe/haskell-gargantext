{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.NodeNode where

import Prelude
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS

import qualified Opaleye as O
import Opaleye (Column, PGBool, PGInt4, PGText, PGTimestamptz, PGFloat8
               , Table(Table), Query
               , QueryRunnerColumnDefault, queryRunnerColumnDefault 
               , fieldQueryRunnerColumn 
               , (.==), (.>)
               , required, optional
               )

import Data.Gargantext.Database.Private (infoGargandb)
import Data.Gargantext.Database.Instances

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


nodeNodeTable :: O.Table NodeNodeWrite NodeNodeRead
nodeNodeTable  = O.Table "nodes_nodes" (pNodeNode NodeNode { nodeNode_node1_id = required "node1_id"
                                                           , nodeNode_node2_id = required "node2_id"
                                                           , nodeNode_score    = optional "score"
                                                           }
                                       )


queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = O.queryTable nodeNodeTable


-- | not optimized (get all ngrams without filters)
nodeNodes :: IO [NodeNode]
nodeNodes = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn queryNodeNodeTable

