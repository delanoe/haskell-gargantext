{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Context where

-- TODO Add support for adding FrameWrite comments for a Context

import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  , lift
  )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.NodeContext (getNodeContext)
import Gargantext.Database.Schema.NodeContext (NodeContext, NodeContextPoly(..))
import Gargantext.Prelude
import GHC.Generics (Generic)

data NodeContextGQL = NodeContextGQL
   { nc_id         :: Maybe Int
   , nc_node_id    :: Int
   , nc_context_id :: Int
   , nc_score      :: Maybe Double
   , nc_category   :: Maybe Int
   }
  deriving (Generic, GQLType, Show)

-- | Arguments to the "context node" query.
data NodeContextArgs
  = NodeContextArgs
    { context_id :: Int
    , node_id    :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve context from a query.
resolveNodeContext
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => NodeContextArgs -> GqlM e env [NodeContextGQL]
resolveNodeContext NodeContextArgs { context_id, node_id } = dbNodeContext context_id node_id

-- | Inner function to fetch the node context DB.
dbNodeContext
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> Int -> GqlM e env [NodeContextGQL]
dbNodeContext context_id node_id = do
  -- lift $ printDebug "[dbUsers]" user_id
--  user <- getUsersWithId user_id
--  hyperdata <- getUserHyperdata user_id
--  lift (map toUser <$> zip user hyperdata)
  c <- lift $ getNodeContext (NodeId context_id) (NodeId node_id)
  pure [toNodeContextGQL c]

toNodeContextGQL :: NodeContext -> NodeContextGQL
toNodeContextGQL (NodeContext { _nc_node_id = NodeId nc_node_id
                              , _nc_context_id = NodeId nc_context_id
                              , .. }) =
  NodeContextGQL { nc_id = _nc_id
                 , nc_node_id
                 , nc_context_id
                 , nc_score = _nc_score
                 , nc_category = _nc_category }
