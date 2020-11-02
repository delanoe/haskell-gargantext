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
            => NodeMode -> User -> Cmd err [NodeId]
findListsId mode u = do
  r <- getRootId u
  ns <- map _dt_nodeId <$> filter (\n -> _dt_typeId n == nodeTypeId NodeList)
                       <$> findNodes' mode r
  -- printDebug "findListsIds" ns
  pure ns


findNodes' :: HasTreeError err
          => NodeMode -> RootId
          -> Cmd err [DbTreeNode]
findNodes' Private r = findNodes Private r $ [NodeFolderPrivate] <> commonNodes
findNodes' Shared  r = findNodes Shared  r $ [NodeFolderShared ] <> commonNodes
findNodes' Public  r = findNodes Public  r $ [NodeFolderPublic ] <> commonNodes

commonNodes:: [NodeType]
commonNodes = [NodeFolder, NodeCorpus, NodeList]
