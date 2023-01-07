module Gargantext.API.Members where

import Gargantext.Prelude
import Gargantext.API.Prelude
import Servant
import Data.Text (Text)
import Gargantext.API.Admin.EnvTypes (Env)
import Gargantext.Core.Types (UserId)
import Gargantext.Database.Admin.Types.Node (NodeType(NodeTeam))
import Gargantext.Database.Query.Table.Node (getNodesIdWithType)
import Gargantext.Database.Action.Share (membersOf)

type MembersAPI = Get '[JSON] [Text]

members :: UserId -> ServerT MembersAPI (GargM Env GargError)
members _ = do
  teamNodeIds <- getNodesIdWithType NodeTeam
  map fst $ concatMap membersOf teamNodeIds
