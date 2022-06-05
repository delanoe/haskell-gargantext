{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Team where

import Gargantext.Prelude
import GHC.Generics (Generic)
import Data.Morpheus.Types (GQLType)
import Data.Text

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

todo :: a
todo = undefined

resolveTeam = todo

deleteTeamMembership = todo