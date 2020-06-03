{-|
Module      : Gargantext.Database.Action.Node
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Action.Node
  where

import Protolude

import Gargantext.Core.Types (Name)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Prelude (Cmd)

------------------------------------------------------------------------
-- | TODO mk all others nodes
mkNodeWithParent :: HasNodeError err
                 => NodeType
                 -> Maybe ParentId
                 -> UserId
                 -> Name
                 -> Cmd err [NodeId]
mkNodeWithParent NodeUser (Just _) _   _    = nodeError UserNoParent

------------------------------------------------------------------------
mkNodeWithParent NodeUser Nothing uId name =
  insertNodesWithParentR Nothing [node NodeUser name fake_HyperdataUser Nothing uId]

mkNodeWithParent _ Nothing _ _ = nodeError HasParent
------------------------------------------------------------------------
mkNodeWithParent NodeFolder (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeFolder name hd Nothing uId]
    where
      hd = defaultFolder

mkNodeWithParent NodeFolderPrivate (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderPrivate "Private" hd Nothing uId]
    where
      hd = defaultFolder

mkNodeWithParent NodeFolderShared (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderShared "Shared" hd Nothing uId]
    where
      hd = defaultFolder

mkNodeWithParent NodeFolderPublic (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderPublic "Public" hd Nothing uId]
    where
      hd = defaultFolder

mkNodeWithParent NodeTeam (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeTeam name hd Nothing uId]
    where
      hd = defaultFolder
------------------------------------------------------------------------
mkNodeWithParent NodeCorpus (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeCorpus name hd Nothing uId]
    where
      hd = defaultCorpus

mkNodeWithParent NodeAnnuaire (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeAnnuaire name hd Nothing uId]
    where
      hd = defaultAnnuaire

mkNodeWithParent NodeList (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeList name hd Nothing uId]
    where
      hd = defaultAnnuaire

mkNodeWithParent NodeGraph (Just i) uId _name =
   insertNodesWithParentR (Just i) [node NodeGraph "Graph" hd Nothing uId]
    where
      hd = arbitraryGraph

mkNodeWithParent _ _ _ _       = nodeError NotImplYet

