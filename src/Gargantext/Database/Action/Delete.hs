{-|
Module      : Gargantext.Database.Action.Delete
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO: right managements of nodes children of node Team
-- TODO add proper Right Management Type

TODO: NodeError

-}

module Gargantext.Database.Action.Delete
  where

import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow.Utils (getUserId)
import Gargantext.Database.Admin.Config (hasNodeType)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Admin.Types.Node -- (NodeType(..))
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import qualified Gargantext.Database.Query.Table.Node as N (getNode, deleteNode)
import Gargantext.Database.Action.Share (delFolderTeam)

------------------------------------------------------------------------
deleteNode :: HasNodeError err
           => User
           -> NodeId
           -> Cmd err Int
deleteNode u nodeId = do
  node' <- N.getNode nodeId
  if hasNodeType node' NodeUser
     then panic "Not allowed to delete NodeUser (yet)"
     else if hasNodeType node' NodeTeam
             then do
                uId   <- getUserId u
                if _node_userId node' == uId
                   then N.deleteNode    nodeId
                   else delFolderTeam u nodeId
             else N.deleteNode nodeId



