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

-- TODO: authorization check, list argument
deleteTeamMembership :: (HasConnectionPool env, HasConfig env, HasMail env) => TeamDeleteMArgs -> GqlM' e env [Int]
deleteTeamMembership TeamDeleteMArgs { shared_folder_id, team_node_id } = do
  lift $ deleteMemberShip [(NodeId shared_folder_id, NodeId team_node_id)]