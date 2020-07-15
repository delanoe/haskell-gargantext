{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Frame
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

module Gargantext.Database.Admin.Types.Hyperdata.Frame
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude

------------------------------------------------------------------------
data HyperdataFrame =
  HyperdataFrame { _hf_base     :: !Text
                 , _hf_frame_id :: !Text
                 }
    deriving (Generic)


defaultHyperdataFrame :: HyperdataFrame
defaultHyperdataFrame = HyperdataFrame "" ""

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataFrame
$(makeLenses ''HyperdataFrame)
$(deriveJSON (unPrefix "_hf_") ''HyperdataFrame)

instance Arbitrary HyperdataFrame where
  arbitrary = pure defaultHyperdataFrame

instance FromField HyperdataFrame
  where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataFrame
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance ToSchema HyperdataFrame where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hf_") proxy
    & mapped.schema.description ?~ "Frame Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataFrame

