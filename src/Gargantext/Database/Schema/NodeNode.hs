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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNode where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Control.Lens.TH (makeLenses)
import Data.Maybe (Maybe, catMaybes)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Node (CorpusId, DocId)
import Gargantext.Database.Admin.Utils
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import qualified Opaleye as O


data NodeNodePoly node1_id node2_id score cat
                   = NodeNode { _nn_node1_id   :: !node1_id
                              , _nn_node2_id   :: !node2_id
                              , _nn_score      :: !score
                              , _nn_category   :: !cat
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

type NodeNode = NodeNodePoly NodeId NodeId (Maybe Double) (Maybe Int)

$(makeAdaptorAndInstance "pNodeNode" ''NodeNodePoly)
makeLenses ''NodeNodePoly

nodeNodeTable :: Table NodeNodeWrite NodeNodeRead
nodeNodeTable  = Table "nodes_nodes" (pNodeNode
                                NodeNode { _nn_node1_id = required "node1_id"
                                         , _nn_node2_id = required "node2_id"
                                         , _nn_score    = optional "score"
                                         , _nn_category = optional "category"
                                     }
                                     )

queryNodeNodeTable :: Query NodeNodeRead
queryNodeNodeTable = queryTable nodeNodeTable


-- | not optimized (get all ngrams without filters)
nodesNodes :: Cmd err [NodeNode]
nodesNodes = runOpaQuery queryNodeNodeTable

instance QueryRunnerColumnDefault (Nullable PGInt4)   Int            where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGFloat8) Int            where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGFloat8) Double         where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8            (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4              (Maybe Int)    where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

