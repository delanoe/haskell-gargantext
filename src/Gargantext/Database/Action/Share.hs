{-|
Module      : Gargantext.Database.Action.Share
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Database.Action.Share
  where

import Control.Lens (view)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow.Utils (getUserId)
import Gargantext.Database.Admin.Config (hasNodeType)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Admin.Types.Node -- (NodeType(..))
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.NodeNode (insertNodeNode)
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRoot)
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeNode (NodeNodePoly(..))
import Gargantext.Prelude

------------------------------------------------------------------------
shareNodeWith :: HasNodeError err
              => NodeId
              -> User
              -> Cmd err Int64
shareNodeWith n u = do
  nodeToCheck <- getNode   n
  userIdCheck <- getUserId u
  if not (hasNodeType nodeToCheck NodeTeam)
    then panic "Can share node Team only"
    else if (view node_userId nodeToCheck == userIdCheck)
     then panic "Can share to others only"
     else do 
       r <- map _node_id <$> getRoot u
       s <- case head r of
           Nothing -> panic "no root id"
           Just r' -> findNodesId r' [NodeFolderShared]
       insertNodeNode $ map (\s' -> NodeNode s' n Nothing Nothing) s
------------------------------------------------------------------------

