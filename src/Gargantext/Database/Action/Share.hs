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
import Gargantext.Database.Query.Table.Node (getNode, getNodesWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.NodeNode (insertNodeNode, deleteNodeNode)
import Gargantext.Database.Query.Tree.Root (getRootId)
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
       folderSharedId  <- getFolderSharedId u
       insertNodeNode [NodeNode folderSharedId n Nothing Nothing]
------------------------------------------------------------------------

getFolderSharedId :: User -> Cmd err NodeId
getFolderSharedId u = do
  rootId <- getRootId u
  s <- getNodesWith rootId HyperdataAny (Just NodeFolderShared) Nothing Nothing
  case head s of
    Nothing -> panic "No folder shared found"
    Just  f -> pure (_node_id f)

type TeamId = NodeId

delFolderTeam :: User -> TeamId -> Cmd err Int
delFolderTeam u nId = do
  folderSharedId <- getFolderSharedId u
  deleteNodeNode folderSharedId nId


