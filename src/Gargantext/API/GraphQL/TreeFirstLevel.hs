{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.TreeFirstLevel where

import Gargantext.Prelude
import Data.Morpheus.Types (GQLType, lift, Resolver, QUERY)
import GHC.Generics (Generic)
import Data.Text (Text)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Core.Mail.Types (HasMail)
import qualified Gargantext.Database.Query.Tree as T
import qualified Gargantext.Database.Schema.Node as N
import qualified Gargantext.Database.Admin.Types.Node as NN
import Gargantext.Database.Admin.Types.Node (allNodeTypes, NodeId (NodeId))
import Gargantext.Core.Types (Tree, NodeTree, NodeType)
import Gargantext.Core.Types.Main
    ( Tree(TreeN), _tn_node, _tn_children, NodeTree(NodeTree, _nt_id, _nt_type), _nt_name )
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Admin.Config (fromNodeTypeId)
import Gargantext.Database.Schema.Node (NodePoly(_node_parent_id))

data TreeArgs = TreeArgs
  {
    root_id :: Int
  } deriving (Generic, GQLType)

data TreeNode = TreeNode
  {
    name      :: Text
  , id        :: Int
  , node_type :: NodeType
  , parent_id :: Maybe Int
  } deriving (Generic, GQLType)

data TreeFirstLevel m = TreeFirstLevel
  {
    root     :: TreeNode
  , parent   :: m (Maybe TreeNode)
  , children :: [TreeNode]
  } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

type ParentId = Maybe NodeId

resolveTree :: (HasConnectionPool env, HasConfig env, HasMail env) => TreeArgs -> GqlM e env (TreeFirstLevel (GqlM e env))
resolveTree TreeArgs { root_id } = dbTree root_id

dbTree :: (HasConnectionPool env, HasConfig env, HasMail env) => Int -> GqlM e env (TreeFirstLevel (GqlM e env))
dbTree root_id = do
  let rId = NodeId root_id
  t <- lift $ T.tree T.TreeFirstLevel rId allNodeTypes
  n <- lift $ getNode $ NodeId root_id
  let pId = toParentId n
  pure $ toTree rId pId t
  where
    toParentId N.Node { _node_parent_id } = _node_parent_id


toTree :: (HasConnectionPool env, HasConfig env, HasMail env) => NodeId -> ParentId -> Tree NodeTree -> TreeFirstLevel (GqlM e env)
toTree rId pId TreeN { _tn_node, _tn_children } = TreeFirstLevel
  { parent   = resolveParent pId
  , root     = toTreeNode pId _tn_node
  , children = map childrenToTreeNodes $ zip _tn_children $ repeat rId
  }

toTreeNode :: ParentId -> NodeTree -> TreeNode
toTreeNode pId NodeTree { _nt_name, _nt_id, _nt_type } = TreeNode { name = _nt_name, id = id2int _nt_id, node_type = _nt_type, parent_id = id2int <$> pId}
  where
    id2int :: NodeId -> Int
    id2int (NodeId n) = n

childrenToTreeNodes :: (Tree NodeTree, NodeId) -> TreeNode
childrenToTreeNodes (TreeN {_tn_node}, rId) = toTreeNode (Just rId) _tn_node

resolveParent :: (HasConnectionPool env, HasConfig env, HasMail env) => Maybe NodeId -> GqlM e env (Maybe TreeNode)
resolveParent (Just pId) = do
  node <- lift $ getNode pId
  pure $ nodeToTreeNode node
resolveParent Nothing = pure Nothing


nodeToTreeNode :: NN.Node json -> Maybe TreeNode
nodeToTreeNode N.Node {..} = if (fromNodeTypeId _node_typename /= NN.NodeFolderShared) && (fromNodeTypeId _node_typename /= NN.NodeTeam) 
                             then 
                             Just TreeNode { id        = NN.unNodeId _node_id
                                           , name      = _node_name
                                           , node_type = fromNodeTypeId _node_typename
                                           , parent_id = NN.unNodeId <$> _node_parent_id
                                           }
                             else
                             Nothing
