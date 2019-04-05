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
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Tree (treeDB, TreeError(..), HasTreeError(..), dbTree, toNodeTree, DbTreeNode) where

import Control.Lens (Prism', (#), (^..), at, each, _Just, to)
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Map (Map, fromListWith, lookup)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Gargantext.Prelude
import Gargantext.Core.Types.Main (NodeTree(..), Tree(..))
import Gargantext.Database.Types.Node (NodeId)
import Gargantext.Database.Config (fromNodeTypeId)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
------------------------------------------------------------------------
-- import Gargantext.Database.Utils (runCmdDev)
-- treeTest :: IO (Tree NodeTree)
-- treeTest = runCmdDev $ treeDB 347474
------------------------------------------------------------------------

data TreeError = NoRoot | EmptyRoot | TooManyRoots
  deriving (Show)

class HasTreeError e where
  _TreeError :: Prism' e TreeError

treeError :: (MonadError e m, HasTreeError e) => TreeError -> m a
treeError te = throwError $ _TreeError # te

-- | Returns the Tree of Nodes in Database
treeDB :: HasTreeError err => RootId -> Cmd err (Tree NodeTree)
treeDB r = toTree =<< (toTreeParent <$> dbTree r)

type RootId = NodeId
type ParentId = NodeId
------------------------------------------------------------------------
toTree :: (MonadError e m, HasTreeError e)
       => Map (Maybe ParentId) [DbTreeNode] -> m (Tree NodeTree)
toTree m =
    case lookup Nothing m of
        Just [n] -> pure $ toTree' m n
        Nothing  -> treeError NoRoot
        Just []  -> treeError EmptyRoot
        Just _   -> treeError TooManyRoots

toTree' :: Map (Maybe ParentId) [DbTreeNode] -> DbTreeNode -> Tree NodeTree
toTree' m n =
  TreeN (toNodeTree n) $
    m ^.. at (Just $ dt_nodeId n) . _Just . each . to (toTree' m)

------------------------------------------------------------------------
toNodeTree :: DbTreeNode -> NodeTree
toNodeTree (DbTreeNode nId tId _ n) = NodeTree n nodeType nId
  where
    nodeType = fromNodeTypeId tId
------------------------------------------------------------------------
toTreeParent :: [DbTreeNode] -> Map (Maybe ParentId) [DbTreeNode]
toTreeParent = fromListWith (<>) . map (\n -> (dt_parentId n, [n]))
------------------------------------------------------------------------
data DbTreeNode = DbTreeNode { dt_nodeId :: NodeId
                             , dt_typeId :: Int
                             , dt_parentId :: Maybe NodeId
                             , dt_name     :: Text
                             } deriving (Show)

-- | Main DB Tree function
-- TODO add typenames as parameters
dbTree :: RootId -> Cmd err [DbTreeNode]
dbTree rootId = map (\(nId, tId, pId, n) -> DbTreeNode nId tId pId n) <$> runPGSQuery [sql|
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
        WHERE c.typename IN (2,3,30,31,7,9)
      )
  SELECT * from tree;
  |] (Only rootId)





