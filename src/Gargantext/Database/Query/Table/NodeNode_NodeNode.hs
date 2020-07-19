{-|
Module      : Gargantext.Database.Query.Table.NodeNode_NodeNode
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeNode
  where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Data.Maybe (catMaybes)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import qualified Opaleye as O
import Opaleye

import Gargantext.Core.Types
import Gargantext.Database.Schema.NodeNode_NodeNode
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node (CorpusId, DocId, pgNodeId)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node
import Gargantext.Prelude


queryNodeNode_NodeNodeTable :: Query NodeNode_NodeNodeRead
queryNodeNode_NodeNodeTable = queryTable nodeNode_NodeNodeTable

------------------------------------------------------------------------
insertNodeNode_NodeNode :: [NodeNode_NodeNode] -> Cmd err Int64
insertNodeNode_NodeNode ns = mkCmd $ \conn -> runInsert_ conn
                          $ Insert nodeNode_NodeNodeTable ns' rCount Nothing
  where
    ns' :: [NodeNode_NodeNodeWrite]
    ns' = map (\(NodeNode_NodeNode nn1 nn2 w)
                -> NodeNode_NodeNode (pgInt4 nn1)
                                     (pgInt4 nn1)
                                     (pgDouble <$> x)
              ) ns

------------------------------------------------------------------------
-- | TODO delete
--

------------------------------------------------------------------------
