{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.File
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

module Gargantext.Database.Admin.Types.Hyperdata.File
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude

------------------------------------------------------------------------
data HyperdataFile =
  HyperdataFile { _hff_name     :: !Text
                , _hff_path     :: !Text
                , _hff_mime     :: !Text
                }
    deriving (Generic)


defaultHyperdataFile :: HyperdataFile
defaultHyperdataFile = HyperdataFile "" "" ""

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
-- | Specific Gargantext instance
instance Hyperdata HyperdataFile
makeLenses ''HyperdataFile

-- | All Json instances
$(deriveJSON (unPrefix "_hff_") ''HyperdataFile)

-- | Arbitrary instances for tests
instance Arbitrary HyperdataFile where
  arbitrary = pure defaultHyperdataFile

instance FromField HyperdataFile
  where
    fromField = fromField'

instance DefaultFromField SqlJsonb HyperdataFile
  where
    defaultFromField = fromPGSFromField

instance ToSchema HyperdataFile where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hff_") proxy
    & mapped.schema.description ?~ "File Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataFile
