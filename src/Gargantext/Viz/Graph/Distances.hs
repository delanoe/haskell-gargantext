{-|
Module      : Gargantext.Graph.Distances
Description : Distance management tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Strict            #-}

module Gargantext.Viz.Graph.Distances
  where


import Data.Array.Accelerate
import Gargantext.Viz.Graph.Distances.Matrice (measureConditional, distributional)

data Distance = Conditional | Distributional


measure :: Distance -> Matrix Int -> Matrix Double
measure Conditional    = measureConditional
measure Distributional = distributional




