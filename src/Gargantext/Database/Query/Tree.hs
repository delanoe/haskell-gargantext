{-|
Module      : Gargantext.Database.Tree
Description : Tree of Resource Nodes built from Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Let a Root Node, return the Tree of the Node as a directed acyclic graph
(Tree).

-- TODO delete node, if not owned, then suppress the link only
-- see Action/Delete.hs
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Database.Query.Tree
  ( module Gargantext.Database.Query.Tree.Error
  , isDescendantOf
  , isIn
  , treeDB
  , treeDB'
  , findNodesId
  , DbTreeNode(..)
  , dt_name
  , dt_nodeId
  , dt_typeId
  , findShared
  )
  where

import Control.Lens ((^..), at, each, _Just, to, set, makeLenses)
import Control.Monad.Error.Class (MonadError())
import Data.List (tail, concat)
import Data.Map (Map, fromListWith, lookup)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Gargantext.Core.Types.Main (NodeTree(..), Tree(..))
import Gargantext.Database.Admin.Config (fromNodeTypeId, nodeTypeId)
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType, DocId, allNodeTypes)
import Gargantext.Database.Admin.Types.Node -- (pgNodeId, NodeType(..))
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Query.Table.NodeNode (getNodeNode)
import Gargantext.Database.Query.Tree.Error
import Gargantext.Database.Schema.NodeNode (NodeNodePoly(..))
import Gargantext.Prelude

------------------------------------------------------------------------
data DbTreeNode = DbTreeNode { _dt_nodeId :: NodeId
                             , _dt_typeId :: Int
                             , _dt_parentId :: Maybe NodeId
                             , _dt_name     :: Text
                             } deriving (Show)

makeLenses ''DbTreeNode
------------------------------------------------------------------------
-- | Returns the Tree of Nodes in Database
-- (without shared folders)
-- keeping this for teaching purpose only
treeDB' :: HasTreeError err
       => RootId
       -> [NodeType]
       -> Cmd err (Tree NodeTree)
treeDB' r nodeTypes = 
  (dbTree r nodeTypes <&> toTreeParent) >>= toTree
  -- Same as (but easier to read) :
  -- toTree =<< (toTreeParent <$> dbTree r nodeTypes)

treeDB :: HasTreeError err
       => RootId
       -> [NodeType]
       -> Cmd err (Tree NodeTree)
treeDB r nodeTypes = do
  mainRoot    <- dbTree r nodeTypes
  sharedRoots <- findShared r nodeTypes
  toTree      $ toTreeParent (mainRoot <> sharedRoots)

------------------------------------------------------------------------
-- | Collaborative Nodes in the Tree
findShared :: RootId -> [NodeType] -> Cmd err [DbTreeNode]
findShared r nt = do
  folderSharedId <- maybe (panic "no folder found") identity
                <$> head
                <$> findNodesId r [NodeFolderShared]
  folders <- getNodeNode folderSharedId
  nodesSharedId <- mapM (\child -> sharedTree folderSharedId child nt)
                   $ map _nn_node2_id folders
  pure $ concat nodesSharedId

sharedTree :: ParentId -> NodeId -> [NodeType] -> Cmd err [DbTreeNode]
sharedTree p n nt = dbTree n nt
               <&> map (\n' -> if _dt_nodeId n' == n 
                                  then set dt_parentId (Just p) n'
                                  else n')

-- | findNodesId returns all nodes matching nodeType but the root (Nodeuser)
findNodesId :: RootId -> [NodeType] -> Cmd err [NodeId]
findNodesId r nt = tail
                <$> map _dt_nodeId
                <$> dbTree r nt
------------------------------------------------------------------------
------------------------------------------------------------------------
toTree :: ( MonadError e m
          , HasTreeError e)
       => Map (Maybe ParentId) [DbTreeNode]
       -> m (Tree NodeTree)
toTree m =
    case lookup Nothing m of
        Just [n] -> pure $ toTree' m n
        Nothing  -> treeError NoRoot
        Just []  -> treeError EmptyRoot
        Just _   -> treeError TooManyRoots

     where
      toTree' :: Map (Maybe ParentId) [DbTreeNode]
              -> DbTreeNode
              -> Tree NodeTree
      toTree' m' n =
        TreeN (toNodeTree n) $
          m' ^.. at (Just $ _dt_nodeId n) . _Just . each . to (toTree' m')

      toNodeTree :: DbTreeNode
                 -> NodeTree
      toNodeTree (DbTreeNode nId tId _ n) = NodeTree n nodeType nId
        where
          nodeType = fromNodeTypeId tId
------------------------------------------------------------------------
toTreeParent :: [DbTreeNode]
             -> Map (Maybe ParentId) [DbTreeNode]
toTreeParent = fromListWith (<>) . map (\n -> (_dt_parentId n, [n]))
------------------------------------------------------------------------
-- | Main DB Tree function
dbTree :: RootId
       -> [NodeType]
       -> Cmd err [DbTreeNode]
dbTree rootId nodeTypes = map (\(nId, tId, pId, n) -> DbTreeNode nId tId pId n)
  <$> runPGSQuery [sql|
    WITH RECURSIVE
        tree (id, typename, parent_id, name) AS
        (
          SELECT p.id, p.typename, p.parent_id, p.name
          FROM nodes AS p
          WHERE p.id = ?

          UNION

          SELECT c.id, c.typename, c.parent_id, c.name
          FROM nodes AS c

          INNER JOIN tree AS s ON c.parent_id = s.id
           WHERE c.typename IN ?
        )
    SELECT * from tree;
    |] (rootId, In typename)
  where
    typename = map nodeTypeId ns
    ns = case nodeTypes of
      [] -> allNodeTypes
      _  -> nodeTypes

isDescendantOf :: NodeId -> RootId -> Cmd err Bool
isDescendantOf childId rootId = (== [Only True])
  <$> runPGSQuery [sql|
                  BEGIN ;
                  SET TRANSACTION READ ONLY;
                  COMMIT;

                  WITH RECURSIVE
      tree (id, parent_id) AS
      (
        SELECT c.id, c.parent_id
        FROM nodes AS c
        WHERE c.id = ?

        UNION

        SELECT p.id, p.parent_id
        FROM nodes AS p
        INNER JOIN tree AS t ON t.parent_id = p.id

      )
  SELECT COUNT(*) = 1 from tree AS t
  WHERE t.id = ?;
  |] (childId, rootId)

-- TODO should we check the category?
isIn :: NodeId -> DocId -> Cmd err Bool
isIn cId docId = ( == [Only True])
  <$> runPGSQuery [sql| SELECT COUNT(*) = 1
    FROM nodes_nodes nn
      WHERE nn.node1_id = ?
        AND nn.node2_id = ?;
  |] (cId, docId)
-----------------------------------------------------
