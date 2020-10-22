{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Viz.Types where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Protolude
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)

---------------
-- | Chart | --
---------------

data Chart = ChartHisto | ChartScatter | ChartPie
  deriving (Generic)

-- TODO use UTCTime
data Histo = Histo { histo_dates :: ![Text]
                   , histo_count :: ![Int]
                   }
  deriving (Show, Generic)

instance ToSchema Histo where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "histo_")
instance Arbitrary Histo
  where
    arbitrary = elements [ Histo ["2012"] [1]
                         , Histo ["2013"] [1]
                         ]
deriveJSON (unPrefix "histo_") ''Histo
