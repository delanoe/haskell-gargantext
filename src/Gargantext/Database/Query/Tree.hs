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
  , tree
  , TreeMode(..)
  , findNodesId
  , DbTreeNode(..)
  , dt_name
  , dt_nodeId
  , dt_typeId
  , findShared
  , findNodes
  , findNodesWithType
  , NodeMode(..)

  , sharedTreeUpdate
  , dbTree
  , updateTree
  )
  where

import Control.Lens (view, toListOf, at, each, _Just, to, set, makeLenses)
import Control.Monad.Error.Class (MonadError())
import Data.List (tail, concat, nub)
import qualified Data.List as List
import Data.Map (Map, fromListWith, lookup)
-- import Data.Monoid (mconcat)
import Data.Proxy
-- import qualified Data.Set  as Set
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Gargantext.Prelude
import Gargantext.Core
import Gargantext.Core.Types.Main (NodeTree(..), Tree(..))
import Gargantext.Database.Admin.Config (fromNodeTypeId, nodeTypeId, fromNodeTypeId)
import Gargantext.Database.Admin.Types.Hyperdata.Any (HyperdataAny)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.NodeNode (getNodeNode)
import Gargantext.Database.Query.Tree.Error
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Database.Schema.NodeNode (NodeNodePoly(..))

------------------------------------------------------------------------
data DbTreeNode = DbTreeNode { _dt_nodeId   :: NodeId
                             , _dt_typeId   :: Int
                             , _dt_parentId :: Maybe NodeId
                             , _dt_name     :: Text
                             } deriving (Show)

makeLenses ''DbTreeNode

instance Eq DbTreeNode where
  (==) d1 d2 = (==) (_dt_nodeId d1) (_dt_nodeId d2)

------------------------------------------------------------------------

data TreeMode = TreeBasic | TreeAdvanced | TreeFirstLevel

-- | Returns the Tree of Nodes in Database
tree :: (HasTreeError err, HasNodeError err)
     => TreeMode
     -> RootId
     -> [NodeType]
     -> Cmd err (Tree NodeTree)
tree TreeBasic    = tree_basic
tree TreeAdvanced = tree_advanced
tree TreeFirstLevel = tree_first_level

-- | Tree basic returns the Tree of Nodes in Database
-- (without shared folders)
-- keeping this for teaching purpose only
tree_basic :: (HasTreeError err, HasNodeError err)

           => RootId
           -> [NodeType]
           -> Cmd err (Tree NodeTree)
tree_basic r nodeTypes =
  (dbTree r nodeTypes <&> toTreeParent) >>= toTree
  -- Same as (but easier to read) :
  -- toTree =<< (toTreeParent <$> dbTree r nodeTypes)

-- | Advanced mode of the Tree enables shared nodes
tree_advanced :: (HasTreeError err, HasNodeError err)
              => RootId
              -> [NodeType]
              -> Cmd err (Tree NodeTree)
tree_advanced r nodeTypes = do
  -- let rPrefix s = "[tree_advanced] root = " <> show r <> " " <> s
  mainRoot    <- findNodes r Private nodeTypes
  -- printDebug (rPrefix "mainRoot") mainRoot
  publicRoots <- findNodes r Public  nodeTypes
  -- printDebug (rPrefix "publicRoots") publicRoots
  sharedRoots <- findNodes r Shared  nodeTypes
  -- printDebug (rPrefix "sharedRoots") sharedRoots
  toTree      $ toTreeParent (mainRoot <> sharedRoots <> publicRoots)

-- | Fetch only first level of tree
tree_first_level :: (HasTreeError err, HasNodeError err)
                 => RootId
                 -> [NodeType]
                 -> Cmd err (Tree NodeTree)
tree_first_level r nodeTypes = do
  -- let rPrefix s = mconcat [ "[tree_first_level] root = "
  --                         , show r
  --                         , ", nodeTypes = "
  --                         , show nodeTypes
  --                         , " "
  --                         , s ]
  mainRoot    <- findNodes r Private nodeTypes
  -- printDebug (rPrefix "mainRoot") mainRoot
  publicRoots <- findNodes r Public  nodeTypes
  -- printDebug (rPrefix "publicRoots") publicRoots
  sharedRoots <- findNodes r SharedDirect  nodeTypes
  -- printDebug (rPrefix "sharedRoots") sharedRoots
  ret <- toTree $ toSubtreeParent r (mainRoot <> sharedRoots <> publicRoots)
  -- printDebug (rPrefix "tree") ret
  pure ret

------------------------------------------------------------------------
data NodeMode = Private | Shared | Public | SharedDirect

findNodes :: (HasTreeError err, HasNodeError err)
          => RootId
          -> NodeMode
          -> [NodeType]
          -> Cmd err [DbTreeNode]
findNodes r Private nt       = dbTree r nt
findNodes r Shared  nt       = findShared r NodeFolderShared nt sharedTreeUpdate
findNodes r SharedDirect  nt = findSharedDirect r NodeFolderShared nt sharedTreeUpdate
findNodes r Public  nt       = findShared r NodeFolderPublic nt publicTreeUpdate

------------------------------------------------------------------------
-- | Collaborative Nodes in the Tree
--   Queries the `nodes_nodes` table.
findShared :: HasTreeError err
           => RootId -> NodeType -> [NodeType] -> UpdateTree err
           -> Cmd err [DbTreeNode]
findShared r nt nts fun = do
  foldersSharedId <- findNodesId r [nt]
  trees           <- mapM (updateTree nts fun) foldersSharedId
  pure $ concat trees

-- | Find shared folders with "direct" access, i.e. when fetching only
-- first-level subcomponents. This works in a simplified manner: fetch the node
-- and get the tree for its parent.
findSharedDirect :: (HasTreeError err, HasNodeError err)
                 => RootId -> NodeType -> [NodeType] -> UpdateTree err
                 -> Cmd err [DbTreeNode]
findSharedDirect r nt nts fun = do
  -- let rPrefix s = mconcat [ "[findSharedDirect] r = "
  --                         , show r
  --                         , ", nt = "
  --                         , show nt
  --                         , ", nts = "
  --                         , show nts
  --                         , " "
  --                         , s ]
  parent <- getNodeWith r (Proxy :: Proxy HyperdataAny)
  let mParent = _node_parentId parent
  case mParent of
    Nothing -> pure []
    Just parentId -> do
      foldersSharedId <- findNodesId parentId [nt]
      -- printDebug (rPrefix "foldersSharedId") foldersSharedId
      trees           <- mapM (updateTree nts fun) foldersSharedId
      -- printDebug (rPrefix "trees") trees
      pure $ concat trees


type UpdateTree err = ParentId -> [NodeType] -> NodeId -> Cmd err [DbTreeNode]

updateTree :: HasTreeError err
           => [NodeType] -> UpdateTree err -> RootId
           -> Cmd err [DbTreeNode]
updateTree nts fun r = do
  folders       <- getNodeNode r
  nodesSharedId <- mapM (fun r nts)
                 $ map _nn_node2_id folders
  pure $ concat nodesSharedId


sharedTreeUpdate :: HasTreeError err => UpdateTree err
sharedTreeUpdate p nt n = dbTree n nt
               <&> map (\n' -> if (view dt_nodeId n') == n
                                  -- && elem (fromDBid $ _dt_typeId n') [NodeGraph]
                                  -- && not (elem (fromDBid $ _dt_typeId n') [NodeFile])
                                  then set dt_parentId (Just p) n'
                                  else n')

publicTreeUpdate :: HasTreeError err => UpdateTree err
publicTreeUpdate p nt n = dbTree n nt
               <&> map (\n' -> if _dt_nodeId n' == n
                                  -- && (fromDBid $ _dt_typeId n') /= NodeGraph
                                  -- && not (elem (fromDBid $ _dt_typeId n') [NodeFile])
                                  then set dt_parentId (Just p) n'
                                  else n')



-- | findNodesId returns all nodes matching nodeType but the root (Nodeuser)
findNodesId :: RootId -> [NodeType] -> Cmd err [NodeId]
findNodesId r nt = tail
                <$> map _dt_nodeId
                <$> dbTree r nt

findNodesWithType :: RootId -> [NodeType] -> [NodeType] -> Cmd err [DbTreeNode]
findNodesWithType root target through =
  filter isInTarget <$> dbTree root through
    where
      isInTarget n = List.elem (fromDBid $ view dt_typeId n)
                   $ List.nub $ target <> through

------------------------------------------------------------------------
------------------------------------------------------------------------
toTree :: ( MonadError e m
          , HasTreeError e
          , MonadBase IO m )
       => Map (Maybe ParentId) [DbTreeNode]
       -> m (Tree NodeTree)
toTree m =
    case lookup Nothing m of
        Just [root] -> pure $ toTree' m root
        Nothing     -> treeError NoRoot
        Just []     -> treeError EmptyRoot
        Just _r     -> treeError TooManyRoots

      where
        toTree' :: Map (Maybe ParentId) [DbTreeNode]
                -> DbTreeNode
                -> Tree NodeTree
        toTree' m' root =
          TreeN (toNodeTree root) $
            -- | Lines below are equivalent computationally but not semantically
            -- m' ^.. at (Just $ _dt_nodeId root) . _Just . each . to (toTree' m')
            toListOf (at (Just $ _dt_nodeId root) . _Just . each . to (toTree' m')) m'

        toNodeTree :: DbTreeNode
                   -> NodeTree
        toNodeTree (DbTreeNode nId tId _ n) = NodeTree n (fromNodeTypeId tId) nId

------------------------------------------------------------------------
toTreeParent :: [DbTreeNode]
             -> Map (Maybe ParentId) [DbTreeNode]
toTreeParent = fromListWith (\a b -> nub $ a <> b) . map (\n -> (_dt_parentId n, [n]))
------------------------------------------------------------------------
-- toSubtreeParent' :: [DbTreeNode]
--                 -> Map (Maybe ParentId) [DbTreeNode]
-- toSubtreeParent' ns = fromListWith (\a b -> nub $ a <> b) . map (\n -> (_dt_parentId n, [n])) $ nullifiedParents
--   where
--     nodeIds = Set.fromList $ map (\n -> unNodeId $ _dt_nodeId n) ns
--     nullifiedParents = map nullifyParent ns
--     nullifyParent dt@(DbTreeNode { _dt_parentId = Nothing }) = dt
--     nullifyParent dt@(DbTreeNode { _dt_nodeId = nId
--                                  , _dt_parentId = Just pId
--                                  , _dt_typeId = tId
--                                  , _dt_name = name }) =
--       if Set.member (unNodeId pId) nodeIds then
--         dt
--       else
--         DbTreeNode { _dt_nodeId = nId
--                    , _dt_typeId = tId
--                    , _dt_parentId = Nothing
--                    , _dt_name = name }
------------------------------------------------------------------------
toSubtreeParent :: RootId
                -> [DbTreeNode]
                -> Map (Maybe ParentId) [DbTreeNode]
toSubtreeParent r ns = fromListWith (\a b -> nub $ a <> b) . map (\n -> (_dt_parentId n, [n])) $ nullifiedParents
  where
    nullifiedParents = map nullifyParent ns
    nullifyParent dt@(DbTreeNode { _dt_parentId = Nothing }) = dt
    nullifyParent dt@(DbTreeNode { _dt_nodeId = nId
                                 , _dt_parentId = _pId
                                 , _dt_typeId = tId
                                 , _dt_name = name }) =
      if r == nId then
        DbTreeNode { _dt_nodeId = nId
                   , _dt_typeId = tId
                   , _dt_parentId = Nothing
                   , _dt_name = name }
      else
        dt
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
