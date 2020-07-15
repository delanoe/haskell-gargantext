{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Model
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


module Gargantext.Database.Admin.Types.Hyperdata.Model
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude

------------------------------------------------------------------------
data HyperdataModel =
  HyperdataModel { _hm_params  :: !(Int, Int)
                 , _hm_path    :: !Text
                 , _hm_score   :: !(Maybe Double)
                 } deriving (Show, Generic)

defaultHyperdataModel :: HyperdataModel
defaultHyperdataModel = HyperdataModel (400,500) "data/models/test.model" (Just 0.83)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataModel
$(makeLenses ''HyperdataModel)
$(deriveJSON (unPrefix "_hm_") ''HyperdataModel)

instance Arbitrary HyperdataModel where
  arbitrary = pure defaultHyperdataModel

instance FromField HyperdataModel
  where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataModel
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance ToSchema HyperdataModel where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hm_") proxy
    & mapped.schema.description ?~ "Model Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataModel

