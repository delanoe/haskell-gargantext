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
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core.Methods.Similarities.Accelerate.Conditional (measureConditional)
import Gargantext.Core.Methods.Similarities.Accelerate.Distributional (logDistributional)
-- import Gargantext.Core.Text.Metrics.Count (coocOn)
-- import Gargantext.Core.Viz.Graph.Index
import Gargantext.Prelude (Ord, Eq, Int, Double, Show, map)
import Prelude (Enum, Bounded, minBound, maxBound)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
-- import qualified Data.Map  as Map
import qualified Data.Text as Text 

------------------------------------------------------------------------
data Similarity = Conditional | Distributional
  deriving (Show, Eq)

measure :: Similarity -> Matrix Int -> Matrix Double
measure Conditional    x = measureConditional x
measure Distributional x = logDistributional  x

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

-- Let's take 2 different forms to produce another one:
hello_words :: [[Text]]
hello_words = map (Text.splitOn "-") wrds
  where
    wrds = [ "bio-logie"
              , "socio-logie"
--              , "ana-logie"
--              , "micro-scope"
--                , "micro-phone"
--                , "micro-cosme"
--              --  , "micro-biote"
--              , "tele-scope"
--                , "tele-phone"
--                , "tele-surveillance"
--              , "macro-scope"
--              , "macro-cosme"
--              , "macro-biote"
              ]

{-
hello_matrix ms' = measureConditional
                 $ map2mat Square 0 (Map.size ti)
                 $ toIndex ti ms
  where
    ms = coocOn identity ms'
    (ti, it) = createIndices ms
-}

