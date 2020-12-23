{-|
Module      : Gargantext.Database.Action.Query.Node.User
Description : User Node in Gargantext
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Database.Query.Table.Node.User
  where

import Data.Maybe (fromMaybe)
import Gargantext.Core
import Gargantext.Core.Types (Name)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser(..), defaultHyperdataUser)
import Gargantext.Database.Admin.Types.Node (Node, NodeId(..), UserId, NodeType(..), pgNodeId)
import Gargantext.Database.Prelude -- (fromField', Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Schema.Node -- (Node(..))
import Gargantext.Prelude
import Opaleye (limit)


getNodeUser :: NodeId -> Cmd err (Node HyperdataUser)
getNodeUser nId = do
    fromMaybe (panic $ "Node does not exist: " <> (cs $ show nId)) . headMay
             <$> runOpaQuery (limit 1 $ selectNode (pgNodeId nId))

nodeUserW :: HasDBid NodeType => Maybe Name -> Maybe HyperdataUser -> UserId -> NodeWrite
nodeUserW maybeName maybeHyperdata = node NodeUser name user Nothing
  where
    name = maybe "User" identity maybeName
    user = maybe defaultHyperdataUser identity maybeHyperdata

