{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Metrics where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Vector (Vector)
import qualified Data.Vector as V
import Protolude
import Test.QuickCheck.Arbitrary

import Gargantext.Core.Types (ListType(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger, wellNamedSchema)

----------------------------------------------------------------------------

newtype Metrics = Metrics
  { metrics_data :: Vector Metric}
  deriving (Generic, Show)

instance ToSchema Metrics where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "metrics_")
instance Arbitrary Metrics
  where
    arbitrary = (Metrics . V.fromList) <$> arbitrary

data Metric = Metric
  { m_label :: !Text
  , m_x     :: !Double
  , m_y     :: !Double
  , m_cat   :: !ListType
  } deriving (Generic, Show)

instance ToSchema Metric where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "m_")
instance Arbitrary Metric
  where
    arbitrary = Metric <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

deriveJSON (unPrefix "metrics_") ''Metrics
deriveJSON (unPrefix "m_") ''Metric


newtype ChartMetrics a = ChartMetrics { chartMetrics_data :: a }
  deriving (Generic, Show)

instance (Typeable a, ToSchema a) => ToSchema (ChartMetrics a) where
  declareNamedSchema = wellNamedSchema "chartMetrics_"
instance (Arbitrary a) => Arbitrary (ChartMetrics a)
  where
    arbitrary = ChartMetrics <$> arbitrary

deriveJSON (unPrefix "chartMetrics_") ''ChartMetrics
