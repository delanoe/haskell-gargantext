{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Team where

import Gargantext.Prelude
import GHC.Generics (Generic)
import Data.Morpheus.Types (GQLType, Resolver, QUERY, ResolverM, lift)
import Data.Text ( Text )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Action.Share (membersOf, deleteMemberShip)
import Gargantext.Core.Types (NodeId(..), unNodeId)
import Gargantext.Database.Prelude (HasConnectionPool)
import Gargantext.Database (HasConfig)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.API.GraphQL.Utils (authUser, AuthStatus (Invalid, Valid))
import Gargantext.Database.Schema.Node (NodePoly(Node, _node_id), _node_user_id)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.Database.Query.Table.User (getUsersWithNodeHyperdata)

import qualified Data.Text as T

data TeamArgs = TeamArgs
  { team_node_id :: Int } deriving (Generic, GQLType)

data TeamMember = TeamMember
 { username         :: Text
 , shared_folder_id :: Int
 } deriving (Generic, GQLType)

data TeamDeleteMArgs = TeamDeleteMArgs
  { token :: Text
  , shared_folder_id :: Int
  , team_node_id     :: Int
  } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)
type GqlM' e env a = ResolverM e (GargM env GargError) a

todo :: a
todo = undefined

resolveTeam :: (HasConnectionPool env, HasConfig env, HasMail env) => TeamArgs -> GqlM e env [TeamMember]
resolveTeam TeamArgs { team_node_id } = dbTeam team_node_id

dbTeam :: (HasConnectionPool env, HasConfig env, HasMail env) => Int -> GqlM e env [TeamMember]
dbTeam nodeId = do
  let nId = NodeId nodeId
  res <- lift $ membersOf nId
  pure $ map toTeamMember res
  where
    toTeamMember :: (Text, NodeId) -> TeamMember
    toTeamMember (username, fId)= TeamMember {
      username,
      shared_folder_id = unNodeId fId
    }

-- TODO: list as argument
deleteTeamMembership :: (HasConnectionPool env, HasConfig env, HasMail env, HasSettings env) => TeamDeleteMArgs -> GqlM' e env [Int]
deleteTeamMembership TeamDeleteMArgs { token, shared_folder_id, team_node_id } = do
  teamNode <- lift $ getNode $ NodeId team_node_id
  userNodes <- lift (getUsersWithNodeHyperdata $ uId teamNode)
  case userNodes of
    [] -> panic $ "[deleteTeamMembership] User with id " <> T.pack (show $ uId teamNode) <> " doesn't exist."
    (( _, node_u):_) -> do
      testAuthUser <- lift $ authUser (nId node_u) token
      case testAuthUser of
        Invalid -> panic "[deleteTeamMembership] failed to validate user"
        Valid -> do
          lift $ deleteMemberShip [(NodeId shared_folder_id, NodeId team_node_id)]
  where
    uId Node { _node_user_id } = _node_user_id
    nId Node { _node_id } = _node_id
