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
import Gargantext.Database.Admin.Types.Node (allNodeTypes, NodeId (NodeId))
import Gargantext.Core.Types (Tree, NodeTree, NodeType)
import Gargantext.Core.Types.Main
    ( Tree(TreeN), _tn_node, _tn_children, NodeTree(NodeTree, _nt_id, _nt_type), _nt_name )

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

data TreeFirstLevel  = TreeFirstLevel
  {
    root     :: TreeNode
  , parent   :: Maybe TreeNode
  , children :: [TreeNode]
  } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveTree :: (HasConnectionPool env, HasConfig env, HasMail env) => TreeArgs -> GqlM e env TreeFirstLevel
resolveTree TreeArgs { root_id } = dbTree root_id

dbTree :: (HasConnectionPool env, HasConfig env, HasMail env) => Int -> GqlM e env TreeFirstLevel
dbTree root_id = do
  t <- lift $ T.tree T.TreeFirstLevel (NodeId root_id) allNodeTypes
  pure $ toTree t

toTree :: Tree NodeTree -> TreeFirstLevel
toTree TreeN {_tn_node, _tn_children} = TreeFirstLevel
  { parent   = Nothing -- TODO
  , root     = toTreeNode _tn_node
  , children = map childrenToTreeNodes _tn_children
  }

toTreeNode :: NodeTree -> TreeNode
toTreeNode NodeTree {_nt_name, _nt_id, _nt_type} = TreeNode { name = _nt_name, id = id2int _nt_id, node_type = _nt_type}
  where
    id2int :: NodeId -> Int
    id2int (NodeId n) = n

childrenToTreeNodes :: Tree NodeTree -> TreeNode
childrenToTreeNodes TreeN {_tn_node} = toTreeNode _tn_node
