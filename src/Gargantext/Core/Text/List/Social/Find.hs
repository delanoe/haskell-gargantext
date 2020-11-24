{-|
Module      : Gargantext.Core.Text.List.Social.Find
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.Find
  where

-- findList imports
import Control.Lens (view)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Prelude

------------------------------------------------------------------------
findListsId :: (HasNodeError err, HasTreeError err)
            => User -> NodeMode -> Cmd err [NodeId]
findListsId u mode = do
  rootId <- getRootId u
  ns <- map (view dt_nodeId) <$> filter ((== nodeTypeId NodeList) . (view dt_typeId))
                             <$> findNodes' rootId mode
  pure ns

-- | TODO not clear enough:
-- | Shared is for Shared with me but I am not the owner of it
-- | Private is for all Lists I have created
findNodes' :: HasTreeError err
          => RootId
          -> NodeMode
          -> Cmd err [DbTreeNode]
findNodes' r Private = findNodes r Private $ [NodeFolderPrivate]          <> commonNodes
findNodes' r Shared  = findNodes r Shared  $ [NodeFolderShared, NodeTeam] <> commonNodes
findNodes' r Public  = findNodes r Public  $ [NodeFolderPublic ]          <> commonNodes

commonNodes:: [NodeType]
commonNodes = [NodeFolder, NodeCorpus, NodeList, NodeFolderShared, NodeTeam]
