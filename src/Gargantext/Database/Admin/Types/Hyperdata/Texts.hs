{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Texts
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Database.Admin.Types.Hyperdata.Texts
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude

------------------------------------------------------------------------

data HyperdataTexts =
  HyperdataTexts { _ht_preferences :: !(Maybe Text)
                 }
  deriving (Show, Generic)

defaultHyperdataTexts :: HyperdataTexts
defaultHyperdataTexts = HyperdataTexts Nothing

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataTexts

$(makeLenses ''HyperdataTexts)
$(deriveJSON (unPrefix "_ht_") ''HyperdataTexts)

instance Arbitrary HyperdataTexts where
    arbitrary = pure defaultHyperdataTexts

instance ToSchema HyperdataTexts where
  declareNamedSchema proxy =
    -- genericDeclareNamedSchema (unPrefixSwagger "_ht_") proxy
    pure $ genericNameSchema defaultSchemaOptions proxy mempty
    & schema.description ?~ "Texts Hyperdata"
    & schema.example     ?~ toJSON defaultHyperdataTexts

instance FromField HyperdataTexts where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataTexts
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

