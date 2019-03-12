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

import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Gargantext.Text.Metrics
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Gargantext.Core.Types (Limit, ListType(..))
import Gargantext.Database.Types.Node
import Gargantext.Database.Utils (Cmd)
import Gargantext.API.Ngrams.Tools -- (groupNodesByNgrams, getCoocByNgrams)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Data.Swagger
import qualified Data.Map as Map


data Metrics = Metrics
  { metrics_data :: [Metric]}
  deriving (Generic)

instance ToJSON   Metrics
instance ToSchema Metrics
instance Arbitrary Metrics
  where
    arbitrary = Metrics <$> arbitrary

data Metric = Metric
  { m_label :: !Text
  , m_x     :: !Double
  , m_y     :: !Double
  , m_cat   :: !ListType
  } deriving (Generic)

instance ToJSON   Metric
instance ToSchema Metric
instance Arbitrary Metric
  where
    arbitrary = Metric <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary


