{-|
Module      : Gargantext.Database.NodeNode
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
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.NodeNode where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye


data NodeNodePoly node1_id node2_id score
                   = NodeNode { nodeNode_node1_id   :: node1_id
                              , nodeNode_node2_id   :: node2_id
                              , nodeNode_score :: score
                              } deriving (Show)

type NodeNodeWrite     = NodeNodePoly (Column (Nullable PGInt4)) 
                                      (Column (PGInt4)) 
                                      (Column (Nullable PGFloat8))

type NodeNodeRead      = NodeNodePoly (Column (Nullable PGInt4)) 
                                      (Column (PGInt4)) 
                                      (Column (Nullable PGFloat8))

type NodeNodeReadNull  = NodeNodePoly (Column (Nullable PGInt4)) 
                                      (Column (Nullable PGInt4)) 
                                      (Column (Nullable PGFloat8))

type NodeNode = NodeNodePoly Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
$(makeLensesWith abbreviatedFields   ''NodeNodePoly)

nodeNodeTable :: Table NodeNodeWrite NodeNodeRead 
nodeNodeTable  = Table "nodes_nodes" (pNodeNode NodeNode { nodeNode_node1_id = required "node1_id"
                                         , nodeNode_node2_id = required "node2_id"
                                         , nodeNode_score    = required "score"
                                     }
                                     )

queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = queryTable nodeNodeTable


-- | not optimized (get all ngrams without filters)
nodeNodes :: PGS.Connection -> IO [NodeNode]
nodeNodes conn = runQuery conn queryNodeNodeTable

instance QueryRunnerColumnDefault (Nullable PGInt4) Int where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


