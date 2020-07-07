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
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataAny(..))
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
              -> NodeType
              -> User
              -> Cmd err Int64
shareNodeWith n nt u = do
  nodeToCheck <- getNode   n
  case nt of
    NodeFolderShared -> do
      userIdCheck <- getUserId u
      if not (hasNodeType nodeToCheck NodeTeam)
        then panic "Can share node Team only"
        else
          if (view node_userId nodeToCheck == userIdCheck)
            then panic "Can share to others only"
            else do 
              folderSharedId  <- getFolderId u NodeFolderShared
              insertNodeNode [NodeNode folderSharedId n Nothing Nothing]

    NodeFolderPublic -> if not (hasNodeType nodeToCheck NodeGraph)
                          then panic "Can share node graph only"
                          else do
                            folderId  <- getFolderId (UserDBId $ view node_userId nodeToCheck) NodeFolderPublic
                            insertNodeNode [NodeNode folderId n Nothing Nothing]

    _ -> panic "shareNodeWith not implemented with this NodeType"

------------------------------------------------------------------------
getFolderId :: User -> NodeType -> Cmd err NodeId
getFolderId u nt = do
  rootId <- getRootId u
  s <- getNodesWith rootId HyperdataAny (Just nt) Nothing Nothing
  case head s of
    Nothing -> panic "No folder shared found"
    Just  f -> pure (_node_id f)

type TeamId = NodeId

delFolderTeam :: User -> TeamId -> Cmd err Int
delFolderTeam u nId = do
  folderSharedId <- getFolderId u NodeFolderShared
  deleteNodeNode folderSharedId nId

unPublish :: User -> NodeId -> Cmd err Int
unPublish  u nId = do
  folderId <- getFolderId u NodeFolderPublic
  deleteNodeNode folderId nId


