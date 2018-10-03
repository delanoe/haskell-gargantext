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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Gargantext.Database.Tree (treeDB) where

import Data.Map (Map, fromListWith, lookup)
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Gargantext.Prelude
import Gargantext.Core.Types.Main (NodeTree(..), Tree(..))
import Gargantext.Database.Config (typeId2node)
------------------------------------------------------------------------
-- import Gargantext (connectGargandb)
-- import Control.Monad ((>>=))
-- treeTest :: IO (Tree NodeTree)
-- treeTest = connectGargandb "gargantext.ini" >>= \c -> treeDB c 347474
------------------------------------------------------------------------
-- | Returns the Tree of Nodes in Database
treeDB :: Connection -> RootId -> IO (Tree NodeTree)
treeDB c r = toTree <$> toTreeParent <$> dbTree c r

type RootId = Int
type ParentId = Int
------------------------------------------------------------------------
toTree :: Map (Maybe ParentId) [DbTreeNode] -> Tree NodeTree
toTree m = toTree' m n
  where
    n = case lookup Nothing m of
        Nothing   -> panic $ pack "no root"
        Just []   -> panic $ pack "empty root"
        Just [n'] -> n'
        Just _    -> panic $ pack "too many roots"

toTree' :: Map (Maybe ParentId) [DbTreeNode] -> DbTreeNode -> Tree NodeTree
toTree' m n = case lookup (Just $ dt_nodeId n) m of
         Nothing    -> TreeN (toNodeTree n) []
         Just  ns   -> TreeN (toNodeTree n) (map (toTree' m) ns)

------------------------------------------------------------------------
toNodeTree :: DbTreeNode -> NodeTree
toNodeTree (DbTreeNode nId tId _ n) = NodeTree n nodeType nId
  where
    nodeType = typeId2node tId
------------------------------------------------------------------------
toTreeParent :: [DbTreeNode] -> Map (Maybe ParentId) [DbTreeNode]
toTreeParent = fromListWith (<>) . map (\n -> (dt_parentId n, [n]))
------------------------------------------------------------------------
data DbTreeNode = DbTreeNode { dt_nodeId :: Int
                             , dt_typeId :: Int
                             , dt_parentId :: Maybe Int
                             , dt_name     :: Text
                             } deriving (Show)


dbTree :: Connection -> RootId -> IO [DbTreeNode]
dbTree conn rootId = map (\(nId, tId, pId, n) -> DbTreeNode nId tId pId n) <$> query conn [sql|
  WITH RECURSIVE
      -- starting node(s)
      starting (id, typename, parent_id, name) AS
      (
        SELECT n.id, n.typename, n.parent_id, n.name
        FROM nodes AS n
        WHERE n.parent_id = ?           -- this can be arbitrary
      ),
      descendants (id, typename, parent_id, name) AS
      (
        SELECT id, typename, parent_id, name
        FROM starting 
        UNION ALL
        SELECT n.id, n.typename, n.parent_id, n.name
        FROM nodes AS n JOIN descendants AS d ON n.parent_id = d.id
        where n.typename in (2,3,31)
      ),
      ancestors (id, typename, parent_id, name) AS
      (
        SELECT n.id, n.typename, n.parent_id, n.name
        FROM nodes AS n 
        WHERE n.id IN (SELECT parent_id FROM starting)
        UNION ALL
        SELECT n.id, n.typename, n.parent_id, n.name
        FROM nodes AS n JOIN ancestors AS a ON n.id = a.parent_id
      )
  TABLE ancestors
  UNION ALL
  TABLE descendants ;
  |] (Only rootId)

