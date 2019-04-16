{-|
Module      : Gargantext.API.Metrics
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Metrics API

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Metrics
    where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Time (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Utils
import Gargantext.Core.Types (CorpusId)
import Gargantext.Prelude
import Gargantext.Viz.Chart
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

data Metrics = Metrics
  { metrics_data :: [Metric]}
  deriving (Generic, Show)

instance ToSchema Metrics
instance Arbitrary Metrics
  where
    arbitrary = Metrics <$> arbitrary

data Metric = Metric
  { m_label :: !Text
  , m_x     :: !Double
  , m_y     :: !Double
  , m_cat   :: !ListType
  } deriving (Generic, Show)

instance ToSchema Metric
instance Arbitrary Metric
  where
    arbitrary = Metric <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

deriveJSON (unPrefix "metrics_") ''Metrics
deriveJSON (unPrefix "m_") ''Metric


-------------------------------------------------------------

data ChartMetrics a = ChartMetrics { chartMetrics_data :: a }
  deriving (Generic, Show)

instance (ToSchema a) => ToSchema (ChartMetrics a)
instance (Arbitrary a) => Arbitrary (ChartMetrics a)
  where
    arbitrary = ChartMetrics <$> arbitrary

deriveJSON (unPrefix "chartMetrics_") ''ChartMetrics

-------------------------------------------------------------
instance ToSchema Histo
instance Arbitrary Histo
  where
    arbitrary = elements [ Histo ["2012"] [1]
                         , Histo ["2013"] [1]
                         ]
deriveJSON (unPrefix "histo_") ''Histo


-- TODO add start / end
getChart :: CorpusId -> Maybe UTCTime -> Maybe UTCTime -> Cmd err (ChartMetrics Histo)
getChart cId _start _end = do
  h <- histoData cId
  pure (ChartMetrics h)



{-
data FacetChart = FacetChart { facetChart_time  :: UTCTime'
                             , facetChart_count :: Double
                        }
        deriving (Show, Generic)
$(deriveJSON (unPrefix "facetChart_") ''FacetChart)
instance ToSchema FacetChart

instance Arbitrary FacetChart where
    arbitrary = FacetChart <$> arbitrary <*> arbitrary

-}

