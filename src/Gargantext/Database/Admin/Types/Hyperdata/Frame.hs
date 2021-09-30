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

import Control.Lens
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import qualified Network.Wreq as Wreq

------------------------------------------------------------------------
data HyperdataFrame =
  HyperdataFrame { _hf_base     :: !Text
                 , _hf_frame_id :: !Text
                 }
    deriving (Generic, Show)


defaultHyperdataFrame :: HyperdataFrame
defaultHyperdataFrame = HyperdataFrame "" ""

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
-- | Specific Gargantext instance
instance Hyperdata HyperdataFrame
makeLenses ''HyperdataFrame

-- | All Json instances
$(deriveJSON (unPrefix "_hf_") ''HyperdataFrame)

-- | Arbitrary instances for tests
instance Arbitrary HyperdataFrame where
  arbitrary = pure defaultHyperdataFrame

instance FromField HyperdataFrame
  where
    fromField = fromField'

instance DefaultFromField PGJsonb HyperdataFrame
  where
    defaultFromField = fieldQueryRunnerColumn

instance ToSchema HyperdataFrame where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hf_") proxy
    & mapped.schema.description ?~ "Frame Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataFrame

getHyperdataFrameContents :: HyperdataFrame -> IO Text
getHyperdataFrameContents (HyperdataFrame { _hf_base, _hf_frame_id }) = do
  let path = T.concat [_hf_base, "/", _hf_frame_id, "/download"]
  r <- Wreq.get $ T.unpack path
  pure $ decodeUtf8 $ toStrict $ r ^. Wreq.responseBody
