{-|
Module      : Gargantext.Graph.Distances.Matrix
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Gargantext.Viz.Graph.Distances.Matrice
  where

import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Interpreter

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar (fromArr, Array, Z)

import Data.Maybe (Maybe(Just))
import qualified Gargantext.Prelude as P
import qualified Data.Array.Accelerate.Array.Representation     as Repr

matrix :: Elt c => Int -> [c] -> Matrix c
matrix n l = fromList (Z :. n :. n) l

myMat :: Int -> Matrix Double
myMat n = matrix n [1..]

-- | Two ways to get the rank (as documentation)
rank :: (Matrix Double) -> Int
rank m = arrayRank $ arrayShape m

rank' :: (Matrix Double) -> Int
rank' m = n
  where
    Z :. _ :. n = arrayShape m

-----------------------------------------------------------------------
-- | Conditional Distance

type Rank = Int

proba :: Rank -> Acc (Matrix Double) -> Acc (Matrix Double)
proba r mat = zipWith (/) mat (mkSum r mat)

mkSum :: Rank -> Acc (Matrix Double) -> Acc (Matrix Double)
mkSum r mat = replicate (constant (Z :. (r :: Int) :. All)) 
            $ fold (+) 0 mat


type Matrix' a = Acc (Matrix a)

conditional :: Matrix Double -> (Matrix Double, Matrix Double)
conditional m = (run $ ie (use m), run $ sg (use m))
  where
    r :: Rank
    r = rank' m

    xs :: Matrix' Double -> Matrix' Double
    xs mat = zipWith (-) (proba r mat) (mkSum r $ proba r mat)
    ys :: Acc (Matrix Double) -> Acc (Matrix Double)
    ys mat = zipWith (-) (proba r mat) (mkSum r $ transpose $ proba r mat)
    
    ie :: Matrix' Double -> Matrix' Double
    ie mat = map (\x -> x / (2*n-1)) $ zipWith (+) (xs mat) (ys mat)
    sg :: Acc (Matrix Double) -> Acc (Matrix Double)
    sg mat = map (\x -> x / (2*n-1)) $ zipWith (-) (xs mat) (ys mat)

    n :: Exp Double
    n = P.fromIntegral r
    
    --miniMax m = fold minimum $ fold maximum m




-- filter with threshold
-----------------------------------------------------------------------

-- | Distributional Distance

distributional :: Matrix Double -> Matrix Double
distributional m = run $ filter $ ri (use m)
  where
    n    = rank m
    
    filter  m = zipWith (\a b -> max a b) m (transpose m)
    --miniMax m = fold minimum $ fold maximum m

    ri mat = zipWith (/) mat1 mat2
      where
        mat1 = mkSum n $ zipWith min (mi mat) (mi $ transpose mat)
        mat2 = mkSum n mat
    
    mi    m'  = zipWith (\a b -> max (log $ a/b) 0)  m'
              $ zipWith (/) (crossProduct m') (total m')

    total m'' = replicate (constant (Z :. n :. n)) $ fold (+) 0 $ fold (+) 0 m''
    
    crossProduct m = zipWith (*) (cross m  ) (cross (transpose m))
    cross mat      = zipWith (-) (mkSum n mat) (mat)

