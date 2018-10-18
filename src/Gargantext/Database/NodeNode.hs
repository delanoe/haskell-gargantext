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

import Gargantext.Database.Node (Cmd(..), mkCmd)
import Gargantext.Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

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
nodesNodes :: Cmd [NodeNode]
nodesNodes = mkCmd $ \c -> runQuery c queryNodeNodeTable

instance QueryRunnerColumnDefault (Nullable PGInt4) Int where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGBool (Maybe Bool) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn





