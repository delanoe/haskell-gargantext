{-|
Module      : Gargantext.Core.Methods.Similarities.Accelerate.Conditional
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This module aims at implementig distances of terms context by context is
the same referential of corpus.

Implementation use Accelerate library which enables GPU and CPU computation
See Gargantext.Core.Methods.Graph.Accelerate)

-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Gargantext.Core.Methods.Similarities.Accelerate.Conditional
  where

-- import qualified Data.Foldable as P (foldl1)
-- import Debug.Trace (trace)
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gargantext.Core.Methods.Matrix.Accelerate.Utils
import Gargantext.Core.Methods.Similarities.Accelerate.SpeGen
import qualified Gargantext.Prelude as P


-- * Metrics of proximity
-----------------------------------------------------------------------
-- ** Conditional distance

-- *** Conditional distance (basic)

-- | Conditional distance (basic version)
--
-- 2 main metrics are actually implemented in order to compute the
-- proximity of two terms: conditional and distributional
--
-- Conditional metric is an absolute metric which reflects
-- interactions of 2 terms in the corpus.

-- Filtered with MiniMax.

measureConditional :: Matrix Int -> Matrix Double
measureConditional m =  run $ x $ map fromIntegral $ use m
  where
    x :: Acc (Matrix Double) -> Acc (Matrix Double)
    x mat = matMiniMax $ matProba r mat

    r :: Dim
    r = dim m


-- | To filter the nodes
-- The conditional metric P(i|j) of 2 terms @i@ and @j@, also called
-- "confidence" , is the maximum probability between @i@ and @j@ to see
-- @i@ in the same context of @j@ knowing @j@.
--
-- If N(i) (resp. N(j)) is the number of occurrences of @i@ (resp. @j@)
-- in the corpus and _[n_{ij}\] the number of its occurrences we get:
--
-- \[P_c=max(\frac{n_i}{n_{ij}},\frac{n_j}{n_{ij}} )\]
conditional' :: Matrix Int -> (Matrix GenericityInclusion, Matrix SpecificityExclusion)
conditional' m = ( run $ ie $ map fromIntegral $ use m
                 , run $ sg $ map fromIntegral $ use m
                 )
  where
    x :: Acc (Matrix Double) -> Acc (Matrix Double)
    x mat = (matProba r mat)

    xs :: Acc (Matrix Double) -> Acc (Matrix Double)
    xs mat = let mat' = x mat in zipWith (-) (matSumLin r mat') mat'
    ys :: Acc (Matrix Double) -> Acc (Matrix Double)
    ys mat = let mat' = x mat in zipWith (-) (matSumCol r mat') mat'


    ie :: Acc (Matrix Double) -> Acc (Matrix Double)
    ie mat = map (\x' -> x' / (2*(n-1))) $ zipWith (+) (xs mat) (ys mat)
    sg :: Acc (Matrix Double) -> Acc (Matrix Double)
    sg mat = map (\x' -> x' / (2*(n-1))) $ zipWith (-) (xs mat) (ys mat)

    r :: Dim
    r = dim m

    n :: Exp Double
    n = P.fromIntegral r



