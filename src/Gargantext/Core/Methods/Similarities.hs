{-|
Module      : Gargantext.Graph.Similarities
Description : Similarity management tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE Strict            #-}

module Gargantext.Core.Methods.Similarities
  where

import Data.Aeson
import Data.Array.Accelerate (Matrix)
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Core.Methods.Similarities.Accelerate.Conditional (measureConditional)
import Gargantext.Core.Methods.Similarities.Accelerate.Distributional (logDistributional)
import Gargantext.Prelude (Ord, Eq, Int, Double, Show)
import Prelude (Enum, Bounded, minBound, maxBound)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
data Similarity = Conditional | Distributional
  deriving (Show, Eq)

measure :: Similarity -> Matrix Int -> Matrix Double
measure Conditional    x = measureConditional x
measure Distributional x = y
  where
    y = logDistributional x

------------------------------------------------------------------------
withMetric :: GraphMetric -> Similarity
withMetric Order1 = Conditional
withMetric Order2 = Distributional

------------------------------------------------------------------------
data GraphMetric = Order1 | Order2
    deriving (Generic, Eq, Ord, Enum, Bounded, Show)

instance FromJSON  GraphMetric
instance ToJSON    GraphMetric
instance ToSchema  GraphMetric
instance Arbitrary GraphMetric where
  arbitrary = elements [ minBound .. maxBound ]

------------------------------------------------------------------------

