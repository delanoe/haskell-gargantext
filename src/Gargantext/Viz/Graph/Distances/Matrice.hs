{-|
Module      : Gargantext.Graph.Distances.Matrix
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.

Implementation use Accelerate library :
  * Manuel M. T. Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell, and Vinod Grover.
    [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11].
    In _DAMP '11: Declarative Aspects of Multicore Programming_, ACM, 2011.

  * Trevor L. McDonell, Manuel M. T. Chakravarty, Gabriele Keller, and Ben Lippmeier.
    [Optimising Purely Functional GPU Programs][MCKL13].
    In _ICFP '13: The 18th ACM SIGPLAN International Conference on Functional Programming_, ACM, 2013.

  * Robert Clifton-Everest, Trevor L. McDonell, Manuel M. T. Chakravarty, and Gabriele Keller.
    [Embedding Foreign Code][CMCK14].
    In _PADL '14: The 16th International Symposium on Practical Aspects of Declarative Languages_, Springer-Verlag, LNCS, 2014.

  * Trevor L. McDonell, Manuel M. T. Chakravarty, Vinod Grover, and Ryan R. Newton.
    [Type-safe Runtime Code Generation: Accelerate to LLVM][MCGN15].
    In _Haskell '15: The 8th ACM SIGPLAN Symposium on Haskell_, ACM, 2015.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Viz.Graph.Distances.Matrice
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar (fromArr, Array, Z)

import Data.Maybe (Maybe(Just))
import qualified Gargantext.Prelude as P
import qualified Data.Array.Accelerate.Array.Representation     as Repr

import Gargantext.Text.Metrics.Count


-----------------------------------------------------------------------
-- Test perf.
distriTest = distributional $ myMat 100
-----------------------------------------------------------------------

vector :: Int -> (Array (Z :. Int) Int)
vector n = fromList (Z :. n) [0..n]

matrix :: Elt c => Int -> [c] -> Matrix c
matrix n l = fromList (Z :. n :. n) l

myMat :: Int -> Matrix Int
myMat n = matrix n [1..]

-- | Two ways to get the rank (as documentation)
rank :: (Matrix a) -> Int
rank m = arrayRank $ arrayShape m

-----------------------------------------------------------------------
-- | Conditional Distance

-- | Dimension of a square Matrix
-- How to force use with SquareMatrix ?
type Dim = Int

dim :: (Matrix a) -> Dim
dim m = n
  where
    Z :. _ :. n = arrayShape m
    -- == indexTail (arrayShape m)

proba :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
proba r mat = zipWith (/) mat (mkSum r mat)

mkSum :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
mkSum r mat = replicate (constant (Z :. (r :: Int) :. All)) $ sum mat

-- divByDiag 
divByDiag :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
divByDiag d mat = zipWith (/) mat (replicate (constant (Z :. (d :: Int) :. All)) $ diag mat)

diag :: Elt e => Acc (Matrix e) -> Acc (Vector e)
diag m = backpermute (indexTail (shape m)) (lift1 (\(Z :. x) -> (Z :. x :. (x :: Exp Int)))) m

{-
Metric Specificity and genericity: select terms
   Compute genericity/specificity:
        P(j|i) = N(ij) / N(ii)
        P(i|j) = N(ij) / N(jj)

        Gen(i)  = sum P(i|j) | j /= i) / (N-1)
        Spec(i) = sum P(j|i) | i /= j) / (N-1)

        Genericity(i) = (Gen(i) - Spe(i)) / 2
        Inclusion(i)  = (Spec(i) + Gen(i)) / 2

-}

-- M - M-1 = 0
data SquareMatrix    = SymetricMatrix | NonSymetricMatrix

type SymetricMatrix    = Matrix
type NonSymetricMatrix = Matrix

-- |    Compute genericity/specificity:
---- |        P(i|j) = N(ij) / N(jj)
p_ij :: (Elt e, P.Fractional (Exp e)) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
p_ij m = zipWith (/) m (n_jj m)
  where
    n_jj :: Elt e => Acc (SymetricMatrix e) -> Acc (Matrix e)
    n_jj m = backpermute (shape m)
                         (lift1 ( \(Z :. (i :: Exp Int) :. (j:: Exp Int))
                                   -> (Z :. j :. j)
                                )
                         ) m

--        P(j|i) = N(ij) / N(ii)
p_ji :: (Elt e, P.Fractional (Exp e)) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
p_ji m = zipWith (/) m (n_ii m)
  where
    n_ii :: Elt e => Acc (SymetricMatrix e) -> Acc (Matrix e)
    n_ii m = backpermute (shape m)
                         (lift1 ( \(Z :. (i :: Exp Int) :. (j:: Exp Int))
                                   -> (Z :. i :. i)
                                )
                         ) m


type Matrix' a = Acc (Matrix a)
type InclusionExclusion    = Double
type SpecificityGenericity = Double


miniMax :: Acc (Matrix Double) -> Acc (Matrix Double)
miniMax m = map (\x -> ifThenElse (x > miniMax') x 0) m
  where
    miniMax' = (the $ minimum $ maximum m)

-- | Conditional distance (basic version)
conditional :: Matrix Int -> Matrix Double
conditional m = run (miniMax $ proba (dim m) $ map fromIntegral $ use m)


-- | Conditional distance (advanced version)
conditional' :: Matrix Int -> (Matrix InclusionExclusion, Matrix SpecificityGenericity)
conditional' m = (run $ ie $ map fromIntegral $ use m, run $ sg $ map fromIntegral $ use m)
  where

    ie :: Matrix' Double -> Matrix' Double
    ie mat = map (\x -> x / (2*n-1)) $ zipWith (+) (xs mat) (ys mat)
    sg :: Acc (Matrix Double) -> Acc (Matrix Double)
    sg mat = map (\x -> x / (2*n-1)) $ zipWith (-) (xs mat) (ys mat)

    n :: Exp Double
    n = P.fromIntegral r

    r :: Dim
    r = dim m

    xs :: Matrix' Double -> Matrix' Double
    xs mat = zipWith (-) (proba r mat) (mkSum r $ proba r mat)
    ys :: Acc (Matrix Double) -> Acc (Matrix Double)
    ys mat = zipWith (-) (proba r mat) (mkSum r $ transpose $ proba r mat)

-----------------------------------------------------------------------

-- | Distributional Distance
distributional :: Matrix Int -> Matrix Double
distributional m = run $ miniMax $ ri (map fromIntegral $ use m)
  where
    n    = dim m
    
    filter  m = zipWith (\a b -> max a b) m (transpose m)
    
    ri mat = zipWith (/) mat1 mat2
      where
        mat1 = mkSum n $ zipWith min (mi mat) (mi $ transpose mat)
        mat2 = mkSum n mat
    
    mi    m'  = zipWith (\a b -> max (log $ a/b) 0)  m'
              $ zipWith (/) (crossProduct m') (total m')

    total m'' = replicate (constant (Z :. n :. n)) $ fold (+) 0 $ fold (+) 0 m''
    
    crossProduct m = zipWith (*) (cross m  ) (cross (transpose m))
    cross mat      = zipWith (-) (mkSum n mat) (mat)


int2double :: Matrix Int -> Matrix Double
int2double m = run (map fromIntegral $ use m)

incExcSpeGen' :: Matrix Int -> (Vector Double, Vector Double)
incExcSpeGen' m = (run' ie m, run' sg m)
  where
    run' fun mat = run $ fun $ map fromIntegral $ use mat

    ie :: Acc (Matrix Double) -> Acc (Vector Double)
    ie mat = zipWith (-) (pV mat) (pH mat)
--    
    sg :: Acc (Matrix Double) -> Acc (Vector Double)
    sg mat = zipWith (+) (pV mat) (pH mat)

    n :: Exp Double
    n = constant (P.fromIntegral (dim m) :: Double)
  
    pV :: Acc (Matrix Double) -> Acc (Vector Double)
    pV mat = map (\x -> (x-1)/(n-1)) $ sum $ p_ij mat
    
    pH :: Acc (Matrix Double) -> Acc (Vector Double)
    pH mat = map (\x -> (x-1)/(n-1)) $ sum $ p_ji mat


incExcSpeGen_proba :: Matrix Int -> Matrix Double
incExcSpeGen_proba m = run' pro m
  where
    run' fun mat = run $ fun $ map fromIntegral $ use mat

    pro mat = p_ij mat
