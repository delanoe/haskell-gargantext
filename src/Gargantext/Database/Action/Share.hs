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
import Gargantext.Database.Admin.Config (hasNodeType, isInNodeTypes)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataAny(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Admin.Types.Node -- (NodeType(..))
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node (getNode, getNodesWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError, msg)
import Gargantext.Database.Query.Table.NodeNode (insertNodeNode, deleteNodeNode)
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeNode (NodeNodePoly(..))
import Gargantext.Prelude

-- | TODO move in Config of Gargantext
publicNodeTypes :: [NodeType]
publicNodeTypes = [NodeDashboard, NodeGraph, NodePhylo]

------------------------------------------------------------------------

data ShareNodeWith = ShareNodeWith_User { snwu_nodetype :: NodeType
                                        , snwu_user    :: User }
                   | ShareNodeWith_Node { snwn_nodetype :: NodeType
                                        , snwn_node_id :: NodeId
                                        }

------------------------------------------------------------------------
shareNodeWith :: HasNodeError err
              => ShareNodeWith
              -> NodeId
              -> Cmd err Int64
shareNodeWith (ShareNodeWith_User NodeFolderShared u) n = do
  nodeToCheck <- getNode   n
  userIdCheck <- getUserId u
  if not (hasNodeType nodeToCheck NodeTeam)
    then msg "Can share node Team only"
    else
      if (view node_userId nodeToCheck == userIdCheck)
        then msg "Can share to others only"
        else do
          folderSharedId  <- getFolderId u NodeFolderShared
          insertNodeNode [NodeNode folderSharedId n Nothing Nothing]

shareNodeWith (ShareNodeWith_Node NodeFolderPublic nId) n = do
  nodeToCheck <- getNode n
  if not (isInNodeTypes nodeToCheck publicNodeTypes)
    then msg $ "Can share this nodesTypes only: " <> (cs $ show publicNodeTypes)
    else do
      folderToCheck <- getNode nId
      if hasNodeType folderToCheck NodeFolderPublic
         then insertNodeNode [NodeNode nId n Nothing Nothing]
         else msg "Can share NodeWith NodeFolderPublic only"

shareNodeWith _ _ = msg "shareNodeWith not implemented for this NodeType"

------------------------------------------------------------------------
getFolderId :: HasNodeError err => User -> NodeType -> Cmd err NodeId
getFolderId u nt = do
  rootId <- getRootId u
  s <- getNodesWith rootId HyperdataAny (Just nt) Nothing Nothing
  case head s of
    Nothing -> msg "No folder shared found"
    Just  f -> pure (_node_id f)

------------------------------------------------------------------------
type TeamId = NodeId

delFolderTeam :: HasNodeError err => User -> TeamId -> Cmd err Int
delFolderTeam u nId = do
  folderSharedId <- getFolderId u NodeFolderShared
  deleteNodeNode folderSharedId nId


unPublish :: HasNodeError err
          => ParentId -> NodeId
          -> Cmd err Int
unPublish p n = deleteNodeNode p n

