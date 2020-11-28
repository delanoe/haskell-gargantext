{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.List
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

module Gargantext.Database.Admin.Types.Hyperdata.List
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative

import Gargantext.Prelude
import Gargantext.Core.Viz.Types (Histo(..))
import Gargantext.API.Ngrams.NgramsTree (NgramsTree)
import Gargantext.API.Ngrams.Types (TabType)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metrics)

------------------------------------------------------------------------
data HyperdataList =
  HyperdataList { _hl_chart   :: !(Map TabType (ChartMetrics Histo))
                , _hl_list    :: !(Maybe Text)
                , _hl_pie     :: !(Map TabType (ChartMetrics Histo))
                , _hl_scatter :: !(Map TabType Metrics)
                , _hl_tree    :: !(Map TabType (ChartMetrics [NgramsTree]))
                } deriving (Show, Generic)
  -- HyperdataList { _hl_chart   :: !(Maybe (ChartMetrics Histo))
  --               , _hl_list    :: !(Maybe Text)
  --               , _hl_pie     :: !(Maybe (ChartMetrics Histo))
  --               , _hl_scatter :: !(Maybe Metrics)
  --               , _hl_tree    :: !(Maybe (ChartMetrics [NgramsTree]))
  --               } deriving (Show, Generic)

defaultHyperdataList :: HyperdataList
defaultHyperdataList =
  HyperdataList { _hl_chart   = Map.empty
                , _hl_list    = Nothing
                , _hl_pie     = Map.empty
                , _hl_scatter = Map.empty
                , _hl_tree    = Map.empty
                }

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataList

$(makeLenses ''HyperdataList)
$(deriveJSON (unPrefix "_hl_") ''HyperdataList)


------------------------------------------------------------------------
data HyperdataListCooc =
  HyperdataListCooc { _hlc_preferences :: !Text }
  deriving (Generic)

defaultHyperdataListCooc :: HyperdataListCooc
defaultHyperdataListCooc = HyperdataListCooc ""


instance Hyperdata HyperdataListCooc
$(makeLenses ''HyperdataListCooc)
$(deriveJSON (unPrefix "_hlc_") ''HyperdataListCooc)





instance Arbitrary HyperdataList where
  arbitrary = pure defaultHyperdataList
instance Arbitrary HyperdataListCooc where
  arbitrary = pure defaultHyperdataListCooc


instance FromField HyperdataList
  where
    fromField = fromField'

instance FromField HyperdataListCooc
  where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataList
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
instance QueryRunnerColumnDefault PGJsonb HyperdataListCooc
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


instance ToSchema HyperdataList where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hl_") proxy
    & mapped.schema.description ?~ "List Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataList
instance ToSchema HyperdataListCooc where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hlc_") proxy
    & mapped.schema.description ?~ "List Cooc Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataListCooc

