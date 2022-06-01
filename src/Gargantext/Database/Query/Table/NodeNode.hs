{-| Module      : Gargantext.Database.Select.Table.NodeNode
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
  , getNodeNode
  , insertNodeNode
  , deleteNodeNode
  , selectPublicNodes
  )
  where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import qualified Opaleye as O
import Opaleye

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node
import Gargantext.Prelude


queryNodeNodeTable :: Select NodeNodeRead
queryNodeNodeTable = selectTable nodeNodeTable

-- | not optimized (get all ngrams without filters)
_nodesNodes :: Cmd err [NodeNode]
_nodesNodes = runOpaQuery queryNodeNodeTable

------------------------------------------------------------------------
-- | Basic NodeNode tools
getNodeNode :: NodeId -> Cmd err [NodeNode]
getNodeNode n = runOpaQuery (selectNodeNode $ pgNodeId n)
  where
    selectNodeNode :: Column SqlInt4 -> Select NodeNodeRead
    selectNodeNode n' = proc () -> do
      ns <- queryNodeNodeTable -< ()
      restrict -< _nn_node1_id ns .== n'
      returnA -< ns

------------------------------------------------------------------------
-- TODO (refactor with Children)
{-
getNodeNodeWith :: NodeId -> proxy a -> Maybe NodeType -> Cmd err [a]
getNodeNodeWith pId _ maybeNodeType = runOpaQuery query
  where
    query = selectChildren pId maybeNodeType

    selectChildren :: ParentId
                   -> Maybe NodeType
                   -> Select NodeRead
    selectChildren parentId maybeNodeType = proc () -> do
        row@(Node nId typeName _ parent_id _ _ _) <- queryNodeTable -< ()
        (NodeNode _ n1id n2id _ _) <- queryNodeNodeTable -< ()

        let nodeType = maybe 0 toDBid maybeNodeType
        restrict -< typeName  .== sqlInt4 nodeType

        restrict -< (.||) (parent_id .== (pgNodeId parentId))
                          ( (.&&) (n1id .== pgNodeId parentId)
                                  (n2id .== nId))
        returnA -< row
-}

------------------------------------------------------------------------
insertNodeNode :: [NodeNode] -> Cmd err Int
insertNodeNode ns = mkCmd $ \conn -> fromIntegral <$> (runInsert_ conn
                          $ Insert nodeNodeTable ns' rCount (Just DoNothing))
  where
    ns' :: [NodeNodeWrite]
    ns' = map (\(NodeNode n1 n2 x y)
                -> NodeNode (pgNodeId n1)
                            (pgNodeId n2)
                            (sqlDouble <$> x)
                            (sqlInt4   <$> y)
              ) ns



------------------------------------------------------------------------
type Node1_Id = NodeId
type Node2_Id = NodeId

deleteNodeNode :: Node1_Id -> Node2_Id -> Cmd err Int
deleteNodeNode n1 n2 = mkCmd $ \conn ->
  fromIntegral <$> runDelete_ conn
                  (Delete nodeNodeTable
                          (\(NodeNode n1_id n2_id _ _) -> n1_id .== pgNodeId n1
                                                      .&& n2_id .== pgNodeId n2
                          )
                          rCount
                  )

------------------------------------------------------------------------
selectPublicNodes :: HasDBid NodeType
                  => (Hyperdata a, DefaultFromField SqlJsonb a)
                  => Cmd err [(Node a, Maybe Int)]
selectPublicNodes = runOpaQuery (queryWithType NodeFolderPublic)

queryWithType :: HasDBid NodeType
              => NodeType
              -> O.Select (NodeRead, Column (Nullable SqlInt4))
queryWithType nt = proc () -> do
  (n, nn) <- node_NodeNode -< ()
  restrict -< n^.node_typename .== (sqlInt4 $ toDBid nt)
  returnA  -<  (n, nn^.nn_node2_id)

node_NodeNode :: O.Select (NodeRead, NodeNodeReadNull)
node_NodeNode = leftJoin queryNodeTable queryNodeNodeTable cond
  where
    cond :: (NodeRead, NodeNodeRead) -> Column SqlBool
    cond (n, nn) = nn^.nn_node1_id .== n^.node_id



