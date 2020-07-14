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
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Action.Node
  where

import Gargantext.Core.Types (Name)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Viz.Graph (defaultHyperdataGraph)
import Gargantext.Prelude
import Gargantext.Prelude.Utils (sha)
import Gargantext.Database.Prelude
import Control.Lens (view)
import Gargantext.Config (GargConfig(..))

------------------------------------------------------------------------
-- | TODO mk all others nodes
mkNodeWithParent :: (HasNodeError err)
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
      hd = defaultHyperdataFolder

mkNodeWithParent NodeFolderPrivate (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderPrivate "Private" hd Nothing uId]
    where
      hd = defaultHyperdataFolder

mkNodeWithParent NodeFolderShared (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderShared "Shared" hd Nothing uId]
    where
      hd = defaultHyperdataFolder

mkNodeWithParent NodeFolderPublic (Just i) uId _ =
   insertNodesWithParentR (Just i) [node NodeFolderPublic "Public" hd Nothing uId]
    where
      hd = defaultHyperdataFolder

mkNodeWithParent NodeTeam (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeTeam name hd Nothing uId]
    where
      hd = defaultHyperdataFolder
------------------------------------------------------------------------
mkNodeWithParent NodeCorpus (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeCorpus name hd Nothing uId]
    where
      hd = defaultHyperdataCorpus

mkNodeWithParent NodeAnnuaire (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeAnnuaire name hd Nothing uId]
    where
      hd = defaultHyperdataAnnuaire

mkNodeWithParent NodeList (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeList name hd Nothing uId]
    where
      hd = defaultHyperdataAnnuaire

mkNodeWithParent NodeGraph (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeGraph name hd Nothing uId]
    where
      hd = defaultHyperdataGraph

mkNodeWithParent NodeFrameWrite i u n =
  mkNodeWithParent_ConfigureHyperdata NodeFrameWrite i u n

mkNodeWithParent NodeFrameCalc i u n =
  mkNodeWithParent_ConfigureHyperdata NodeFrameCalc i u n

{-
mkNodeWithParent n (Just i) uId name =
   insertNodesWithParentR (Just i) [node NodeDashboard name (hasDefaultData n) Nothing uId]
-}

mkNodeWithParent _ _ _ _       = nodeError NotImplYet


-- | Sugar to create a node, get his NodeId and update his Hyperdata after
mkNodeWithParent_ConfigureHyperdata :: (HasNodeError err)
                                    => NodeType
                                    -> Maybe ParentId
                                    -> UserId
                                    -> Name
                                    -> Cmd err [NodeId]
mkNodeWithParent_ConfigureHyperdata NodeFrameWrite (Just i) uId name =
  mkNodeWithParent_ConfigureHyperdata' NodeFrameWrite (Just i) uId name

mkNodeWithParent_ConfigureHyperdata NodeFrameCalc (Just i) uId name =
  mkNodeWithParent_ConfigureHyperdata' NodeFrameCalc (Just i) uId name

mkNodeWithParent_ConfigureHyperdata    _ _ _ _ = nodeError NotImplYet


-- | Function not exposed
mkNodeWithParent_ConfigureHyperdata' :: (HasNodeError err)
                                    => NodeType
                                    -> Maybe ParentId
                                    -> UserId
                                    -> Name
                                    -> Cmd err [NodeId]
mkNodeWithParent_ConfigureHyperdata' nt (Just i) uId name = do
  maybeNodeId <- insertNodesWithParentR (Just i) [node nt name defaultFolder Nothing uId]
  case maybeNodeId of
    []  -> nodeError (DoesNotExist i)
    [n] -> do
      config <- view hasConfig
      u <- case nt of
            NodeFrameWrite -> pure $ _gc_frame_write_url config
            NodeFrameCalc  -> pure $ _gc_frame_calc_url config
            _              -> nodeError NeedsConfiguration
      let
        s = _gc_secretkey config
        hd = HyperdataFrame u (sha $ s <> (cs $ show n))
      _ <- updateHyperdata n hd
      pure [n]
    (_:_:_)  -> nodeError MkNode
mkNodeWithParent_ConfigureHyperdata' _ _ _ _ = nodeError HasParent

