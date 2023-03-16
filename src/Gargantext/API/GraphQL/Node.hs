{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Node where

import Data.Either (Either(..))
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  , lift
  )
import Data.Text (Text)
import qualified Data.Text as T
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Admin.Types.Node (NodeId(..), NodeType)
import qualified Gargantext.Database.Admin.Types.Node as NN
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, getNode)
import Gargantext.Database.Prelude (CmdCommon)
import qualified Gargantext.Database.Schema.Node as N
import Gargantext.Prelude
import GHC.Generics (Generic)
import qualified Prelude
import Text.Read (readEither)

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
  :: (CmdCommon env)
  => NodeArgs -> GqlM e env [Node]
resolveNodes NodeArgs { node_id } = dbNodes node_id

dbNodes
  :: (CmdCommon env)
  => Int -> GqlM e env [Node]
dbNodes node_id = do
  node <- lift $ getNode $ NodeId node_id
  pure [toNode node]

data NodeParentArgs
  = NodeParentArgs
    { node_id     :: Int
    , parent_type :: Text
    } deriving (Generic, GQLType)

resolveNodeParent
  :: (CmdCommon env)
  => NodeParentArgs -> GqlM e env [Node]
resolveNodeParent NodeParentArgs { node_id, parent_type } = dbParentNodes node_id parent_type

dbParentNodes
  :: (CmdCommon env)
  => Int -> Text -> GqlM e env [Node]
dbParentNodes node_id parent_type = do
  let mParentType = readEither (T.unpack parent_type) :: Either Prelude.String NodeType
  case mParentType of
    Left err -> do
      lift $ printDebug "[dbParentNodes] error reading parent type" (T.pack err)
      pure []
    Right parentType -> do
      mNodeId <- lift $ getClosestParentIdByType (NodeId node_id) parentType -- (fromNodeTypeId parent_type_id)
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
