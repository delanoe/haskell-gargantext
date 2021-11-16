
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Node where

import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  , lift
  )
import Data.Text (Text)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Config (fromNodeTypeId)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import qualified Gargantext.Database.Admin.Types.Node as NN
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, getNode)
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import qualified Gargantext.Database.Schema.Node as N
import Gargantext.Prelude
import GHC.Generics (Generic)

data Node = Node
  { id        :: Int
  , name      :: Text
  , parent_id :: Maybe Int
  , type_id   :: Int
  } deriving (Show, Generic, GQLType)

data NodeArgs
  = NodeArgs
    { node_id        :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve user from a query.
resolveNodes
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => NodeArgs -> GqlM e env [Node]
resolveNodes NodeArgs { node_id } = dbNodes node_id

dbNodes
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> GqlM e env [Node]
dbNodes node_id = do
  node <- lift $ getNode $ NodeId node_id
  pure [toNode node]

data NodeParentArgs
  = NodeParentArgs
    { node_id        :: Int
    , parent_type_id :: Int
    } deriving (Generic, GQLType)

resolveNodeParent
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => NodeParentArgs -> GqlM e env [Node]
resolveNodeParent NodeParentArgs { node_id, parent_type_id } = dbParentNodes node_id parent_type_id

dbParentNodes
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> Int -> GqlM e env [Node]
dbParentNodes node_id parent_type_id = do
  mNodeId <- lift $ getClosestParentIdByType (NodeId node_id) (fromNodeTypeId parent_type_id)
  case mNodeId of
    Nothing -> pure []
    Just id -> do
      node <- lift $ getNode id
      pure [toNode node]

toNode :: NN.Node json -> Node
toNode (N.Node { .. }) = Node { id = NN.unNodeId _node_id
                              , name = _node_name
                              , parent_id = NN.unNodeId <$> _node_parent_id
                              , type_id = _node_typename }
