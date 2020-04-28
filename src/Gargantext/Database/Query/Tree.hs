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

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Query.Tree
  where

import Control.Lens (Prism', (#), (^..), at, each, _Just, to)
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Map (Map, fromListWith, lookup)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Gargantext.Core.Types.Main (NodeTree(..), Tree(..))
import Gargantext.Database.Admin.Types.Node -- (pgNodeId, NodeType(..))
import Gargantext.Database.Admin.Config (fromNodeTypeId, nodeTypeId)
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType, DocId, allNodeTypes)
import Gargantext.Database.Admin.Utils (Cmd, runPGSQuery)
import Gargantext.Prelude

------------------------------------------------------------------------
-- TODO more generic find fun
findCorpus :: RootId -> Cmd err (Maybe CorpusId)
findCorpus r = do
  _mapNodes <- toTreeParent <$> dbTree r []
  pure Nothing

------------------------------------------------------------------------
data TreeError = NoRoot | EmptyRoot | TooManyRoots
  deriving (Show)

class HasTreeError e where
  _TreeError :: Prism' e TreeError

treeError :: ( MonadError e m
             , HasTreeError e)
             => TreeError
             -> m a
treeError te = throwError $ _TreeError # te

-- | Returns the Tree of Nodes in Database
treeDB :: HasTreeError err
       => RootId
       -> [NodeType]
       -> Cmd err (Tree NodeTree)
treeDB r nodeTypes = toTree =<< (toTreeParent <$> dbTree r nodeTypes)

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

toTree' :: Map (Maybe ParentId) [DbTreeNode]
        -> DbTreeNode
        -> Tree NodeTree
toTree' m n =
  TreeN (toNodeTree n) $
    m ^.. at (Just $ dt_nodeId n) . _Just . each . to (toTree' m)

------------------------------------------------------------------------
toNodeTree :: DbTreeNode
           -> NodeTree
toNodeTree (DbTreeNode nId tId _ n) = NodeTree n nodeType nId
  where
    nodeType = fromNodeTypeId tId
------------------------------------------------------------------------
toTreeParent :: [DbTreeNode]
             -> Map (Maybe ParentId) [DbTreeNode]
toTreeParent = fromListWith (<>) . map (\n -> (dt_parentId n, [n]))
------------------------------------------------------------------------
data DbTreeNode = DbTreeNode { dt_nodeId :: NodeId
                             , dt_typeId :: Int
                             , dt_parentId :: Maybe NodeId
                             , dt_name     :: Text
                             } deriving (Show)

-- | Main DB Tree function
-- TODO add typenames as parameters
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
      -- [2, 20, 21, 22, 3, 5, 30, 31, 40, 7, 9, 90, 71]
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


