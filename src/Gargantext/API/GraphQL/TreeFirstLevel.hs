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
  } deriving (Generic, GQLType)

data TreeFirstLevel m = TreeFirstLevel
  {
    root     :: TreeNode
  , parent   :: m (Maybe TreeNode)
  , children :: [TreeNode]
  } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveTree :: (HasConnectionPool env, HasConfig env, HasMail env) => TreeArgs -> GqlM e env (TreeFirstLevel (GqlM e env))
resolveTree TreeArgs { root_id } = dbTree root_id

dbTree :: (HasConnectionPool env, HasConfig env, HasMail env) => Int -> GqlM e env (TreeFirstLevel (GqlM e env))
dbTree root_id = do
  t <- lift $ T.tree T.TreeFirstLevel (NodeId root_id) allNodeTypes
  n <- lift $ getNode $ NodeId root_id
  let pId = toParentId n
  pure $ toTree pId t
  where
    toParentId N.Node { _node_parent_id } = _node_parent_id


toTree :: (HasConnectionPool env, HasConfig env, HasMail env) => Maybe NodeId -> Tree NodeTree -> TreeFirstLevel (GqlM e env)
toTree pId TreeN { _tn_node, _tn_children } = TreeFirstLevel
  { parent   = resolveParent pId -- TODO
  , root     = toTreeNode _tn_node
  , children = map childrenToTreeNodes _tn_children
  }

toTreeNode :: NodeTree -> TreeNode
toTreeNode NodeTree { _nt_name, _nt_id, _nt_type } = TreeNode { name = _nt_name, id = id2int _nt_id, node_type = _nt_type }
  where
    id2int :: NodeId -> Int
    id2int (NodeId n) = n

childrenToTreeNodes :: Tree NodeTree -> TreeNode
childrenToTreeNodes TreeN {_tn_node} = toTreeNode _tn_node

resolveParent :: (HasConnectionPool env, HasConfig env, HasMail env) => Maybe NodeId -> GqlM e env (Maybe TreeNode)
resolveParent (Just pId) = do
  node <- lift $ getNode pId
  pure $ Just $ nodeToTreeNode node
resolveParent Nothing = pure Nothing


nodeToTreeNode :: NN.Node json -> TreeNode
nodeToTreeNode N.Node {..} = TreeNode { id        = NN.unNodeId _node_id
                                      , name      = _node_name
                                      , node_type = fromNodeTypeId _node_typename
                                      }
