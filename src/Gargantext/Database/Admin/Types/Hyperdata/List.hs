{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.List
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


module Gargantext.Database.Admin.Types.Hyperdata.List
  where

import Gargantext.Prelude
import Gargantext.Viz.Types (Histo(..))
import Gargantext.API.Ngrams.NTree (MyTree)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metrics)

------------------------------------------------------------------------
data HyperdataList =
  HyperdataList { hd_chart   :: !(Maybe (ChartMetrics Histo))
                , hd_list    :: !(Maybe Text)
                , hd_pie     :: !(Maybe (ChartMetrics Histo))
                , hd_scatter :: !(Maybe Metrics)
                , hd_tree    :: !(Maybe (ChartMetrics [MyTree]))
                } deriving (Show, Generic)
$(deriveJSON (unPrefix "hd_") ''HyperdataList)

instance Hyperdata HyperdataList

defaultHyperdataList :: HyperdataList
defaultHyperdataList = HyperdataList Nothing Nothing Nothing Nothing Nothing

                      ----
data HyperdataListModel =
  HyperdataListModel { _hlm_params  :: !(Int, Int)
                     , _hlm_path    :: !Text
                     , _hlm_score   :: !(Maybe Double)
                     } deriving (Show, Generic)

instance Hyperdata HyperdataListModel
instance Arbitrary HyperdataListModel where
  arbitrary = elements [HyperdataListModel (100,100) "models/example.model" Nothing]

$(deriveJSON (unPrefix "_hlm_") ''HyperdataListModel)
$(makeLenses ''HyperdataListModel)

defaultHyperdataListModel :: HyperdataListModel
defaultHyperdataListModel = HyperdataListModel (400,500) "data/models/test.model" (Just 0.83)


------------------------------------------------------------------------
data HyperdataScore = HyperdataScore { hyperdataScore_preferences   :: !(Maybe Text)
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataScore_") ''HyperdataScore)

instance Hyperdata HyperdataScore

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance FromField HyperdataList
  where
    fromField = fromField'

instance FromField HyperdataListModel
  where
    fromField = fromField'
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataList
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataListModel
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

