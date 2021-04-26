{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Phylo
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

module Gargantext.Database.Admin.Types.Hyperdata.Phylo
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Core.Viz.LegacyPhylo (Phylo(..))

------------------------------------------------------------------------

data HyperdataPhylo =
  HyperdataPhylo    { _hp_preferences :: !(Maybe Text)
                    , _hp_data        :: !(Maybe Phylo)
                    }
  deriving (Show, Generic)

defaultHyperdataPhylo :: HyperdataPhylo
defaultHyperdataPhylo = HyperdataPhylo Nothing Nothing

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataPhylo

$(makeLenses ''HyperdataPhylo)
$(deriveJSON (unPrefix "_hp_") ''HyperdataPhylo)

instance Arbitrary HyperdataPhylo where
    arbitrary = pure defaultHyperdataPhylo

instance ToSchema HyperdataPhylo where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hp_") proxy
    & mapped.schema.description ?~ "Phylo Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataPhylo

instance FromField HyperdataPhylo where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataPhylo
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
