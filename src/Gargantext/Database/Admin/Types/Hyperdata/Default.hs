{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Default
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.Default
  where

import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Hyperdata.Prelude


data DefaultHyperdata =
    DefaultUser    HyperdataUser 
  | DefaultContact HyperdataContact

  | DefaultCorpus    HyperdataCorpus
  | DefaultCorpusV3  HyperdataCorpus
  | DefaultAnnuaire  HyperdataAnnuaire

  | DefaultDocument  HyperdataDocument
  | DefaultTexts     HyperdataTexts
  | DefaultList      HyperdataList
  | DefaultListCooc  HyperdataListCooc
  | DefaultModel     HyperdataModel

  | DefaultFolder        HyperdataFolder
  | DefaultFolderPrivate HyperdataFolderPrivate
  | DefaultFolderShared  HyperdataFolderShared
  | DefaultTeam          HyperdataFolder
  | DefaultFolderPublic  HyperdataFolderPublic

  | DefaultGraph         HyperdataGraph
  | DefaultPhylo         HyperdataPhylo
  | DefaultDashboard     HyperdataDashboard

  | DefaultFrameWrite    HyperdataFrame
  | DefaultFrameCalc     HyperdataFrame

instance Hyperdata DefaultHyperdata

instance ToJSON DefaultHyperdata where
  toJSON (DefaultUser    x) = toJSON x
  toJSON (DefaultContact x) = toJSON x

  toJSON (DefaultCorpus   x) = toJSON x
  toJSON (DefaultCorpusV3 x) = toJSON x
  toJSON (DefaultAnnuaire x) = toJSON x

  toJSON (DefaultDocument x) = toJSON x
  toJSON (DefaultTexts    x) = toJSON x
  toJSON (DefaultList     x) = toJSON x
  toJSON (DefaultListCooc x) = toJSON x
  toJSON (DefaultModel    x) = toJSON x

  toJSON (DefaultFolder        x) = toJSON x
  toJSON (DefaultFolderPrivate x) = toJSON x
  toJSON (DefaultFolderShared  x) = toJSON x
  toJSON (DefaultTeam          x) = toJSON x
  toJSON (DefaultFolderPublic  x) = toJSON x

  toJSON (DefaultGraph     x) = toJSON x
  toJSON (DefaultPhylo     x) = toJSON x
  toJSON (DefaultDashboard x) = toJSON x

  toJSON (DefaultFrameWrite x) = toJSON x
  toJSON (DefaultFrameCalc  x) = toJSON x


defaultHyperdata :: NodeType -> DefaultHyperdata
defaultHyperdata NodeUser           = DefaultUser    defaultHyperdataUser
defaultHyperdata NodeContact        = DefaultContact defaultHyperdataContact

defaultHyperdata NodeCorpus         = DefaultCorpus   defaultHyperdataCorpus
defaultHyperdata NodeCorpusV3       = DefaultCorpusV3 defaultHyperdataCorpus
defaultHyperdata NodeAnnuaire       = DefaultAnnuaire defaultHyperdataAnnuaire

defaultHyperdata NodeDocument       = DefaultDocument defaultHyperdataDocument
defaultHyperdata NodeTexts          = DefaultTexts    defaultHyperdataTexts
defaultHyperdata NodeList           = DefaultList     defaultHyperdataList
defaultHyperdata NodeListCooc       = DefaultListCooc defaultHyperdataListCooc
defaultHyperdata NodeModel          = DefaultModel    defaultHyperdataModel

defaultHyperdata NodeFolder         = DefaultFolder        defaultHyperdataFolder
defaultHyperdata NodeFolderPrivate  = DefaultFolderPrivate defaultHyperdataFolderPrivate
defaultHyperdata NodeFolderShared   = DefaultFolderShared  defaultHyperdataFolderShared
defaultHyperdata NodeTeam           = DefaultTeam          defaultHyperdataFolder
defaultHyperdata NodeFolderPublic   = DefaultFolderPublic  defaultHyperdataFolderPublic

defaultHyperdata NodeGraph          = DefaultGraph     defaultHyperdataGraph
defaultHyperdata NodePhylo          = DefaultPhylo     defaultHyperdataPhylo
defaultHyperdata NodeDashboard      = DefaultDashboard defaultHyperdataDashboard

defaultHyperdata NodeFrameWrite     = DefaultFrameWrite defaultHyperdataFrame
defaultHyperdata NodeFrameCalc      = DefaultFrameCalc  defaultHyperdataFrame
