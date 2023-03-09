{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Team where

import Gargantext.Prelude
import GHC.Generics (Generic)
import Data.Morpheus.Types (GQLType, Resolver, QUERY, ResolverM, lift)
import Data.Text ( Text )
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.GraphQL.Utils (authUser, AuthStatus (Invalid, Valid))
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.Types (NodeId(..), unNodeId)
import Gargantext.Database (HasConfig)
import Gargantext.Database.Action.Share (membersOf, deleteMemberShip)
import Gargantext.Database.Prelude (HasConnectionPool)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Table.User (getUsersWithNodeHyperdata)
import Gargantext.Database.Schema.Node (NodePoly(Node, _node_id), _node_user_id)

import qualified Data.Text as T
import Gargantext.Database.Schema.User (UserLight(..))

data TeamArgs = TeamArgs
  { team_node_id :: Int } deriving (Generic, GQLType)

data Team = Team
 { team_owner_username :: Text
 , team_members         :: [TeamMember]
 } deriving (Generic, GQLType)

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


resolveTeam :: (HasConnectionPool env, HasConfig env, HasMail env) => TeamArgs -> GqlM e env Team
resolveTeam TeamArgs { team_node_id } = dbTeam team_node_id

dbTeam :: (HasConnectionPool env, HasConfig env, HasMail env) => Int -> GqlM e env Team
dbTeam nodeId = do
  let nId = NodeId nodeId
  res <- lift $ membersOf nId
  teamNode <- lift $ getNode nId
  userNodes <- lift $ getUsersWithNodeHyperdata $ uId teamNode
  let username = getUsername userNodes
  pure $ Team { team_owner_username = username
              , team_members = map toTeamMember res
              }
  where
    toTeamMember :: (Text, NodeId) -> TeamMember
    toTeamMember (username, fId)= TeamMember {
      username,
      shared_folder_id = unNodeId fId
    }
    uId Node { _node_user_id } = _node_user_id
    getUsername [] = panic "[resolveTeam] Team creator doesn't exist"
    getUsername ((UserLight {userLight_username}, _):_) = userLight_username

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
