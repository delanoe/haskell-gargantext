{-|
Module      : Gargantext.Core.Methods.Matrix.Accelerate.Utils
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This module aims at implementig distances of terms context by context is
the same referential of corpus.

Implementation use Accelerate library which enables GPU and CPU computation:

  * Manuel M. T. Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell, and Vinod Grover.
    [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11].
    In _DAMP '11: Declarative Aspects of Multicore Programming_, ACM, 2011.

  * Trevor L. McDonell, Manuel M. T. Chakravarty, Vinod Grover, and Ryan R. Newton.
    [Type-safe Runtime Code Generation: Accelerate to LLVM][MCGN15].
    In _Haskell '15: The 8th ACM SIGPLAN Symposium on Haskell_, ACM, 2015.

-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Gargantext.Core.Methods.Matrix.Accelerate.Utils
  where

import qualified Data.Foldable as P (foldl1)
import Debug.Trace (trace)
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar (Elt(..), Shape(..), Slice(..), (:.))
import Data.Array.Accelerate.Smart (Exp(..))
import Data.Array.Accelerate.Interpreter (run)
import qualified Gargantext.Prelude as P
import Data.Array.Accelerate.LinearAlgebra hiding (Matrix, transpose, Vector)

-----------------------------------------------------------------------
-- | Main operators
-- Matrix Multiplication
(#*#) :: ( Shape ix
        , Slice ix
        , Elt a
        , P.Num (Exp a)
        )
     => Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
(#*#) = multiplyMatrixMatrix


-- | Matrix cell by cell multiplication
(.*) :: ( Shape ix
        , Slice ix
        , Elt a
        , P.Num (Exp a)
        )
     => Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
(.*) = zipWith (*)

(./) :: ( Shape ix
        , Slice ix
        , Elt a
        , P.Num (Exp a)
        , P.Fractional (Exp a)
        )
     => Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
     -> Acc (Array ((ix :. Int) :. Int) a)
(./) = zipWith (/)


-----------------------------------------------------------------------
matrixOne :: Num a => Dim -> Acc (Matrix a)
matrixOne n' = ones
  where
    ones  = fill (index2 n n) 1
    n     = constant n'


matrixIdentity :: Num a => Dim -> Acc (Matrix a)
matrixIdentity n' =
        let zeros = fill (index2 n n) 0
            ones  = fill (index1 n)   1
            n = constant n'
        in
        permute const zeros (\(unindex1 -> i) -> index2 i i) ones


matrixEye :: Num a => Dim -> Acc (Matrix a)
matrixEye n' =
        let ones   = fill (index2 n n) 1
            zeros  = fill (index1 n)   0
            n = constant n'
        in
        permute const ones (\(unindex1 -> i) -> index2 i i) zeros


diagNull :: Num a => Dim -> Acc (Matrix a) -> Acc (Matrix a)
diagNull n m = zipWith (*) m (matrixEye n)

-----------------------------------------------------------------------
_runExp :: Elt e => Exp e -> e
_runExp e = indexArray (run (unit e)) Z

-----------------------------------------------------------------------
-- | Define a vector
--
-- >>> vector 3
-- Vector (Z :. 3) [0,1,2]
vector :: Elt c => Int -> [c] -> (Array (Z :. Int) c)
vector n l = fromList (Z :. n) l

-- | Define a matrix
--
-- >>> matrix 3 ([1..] :: [Double])
-- Matrix (Z :. 3 :. 3)
--   [ 1.0, 2.0, 3.0,
--     4.0, 5.0, 6.0,
--     7.0, 8.0, 9.0]
matrix :: Elt c => Int -> [c] -> Matrix c
matrix n l = fromList (Z :. n :. n) l

-- | Two ways to get the rank (as documentation)
--
-- >>> rank (matrix 3 ([1..] :: [Int]))
-- 2
rank :: (Matrix a) -> Int
rank m = arrayRank $ arrayShape m

-----------------------------------------------------------------------
-- | Dimension of a square Matrix
-- How to force use with SquareMatrix ?
type Dim = Int

-- | Get Dimension of a square Matrix
--
-- >>> dim (matrix 3 ([1..] :: [Int]))
-- 3
dim :: Matrix a -> Dim
dim m = n
  where
    Z :. _ :. n = arrayShape m
    -- indexTail (arrayShape m)

-----------------------------------------------------------------------

-- | Sum of a Matrix by Column
--
-- >>> run $ matSumCol 3 (use $ matrix 3 [1..])
-- Matrix (Z :. 3 :. 3)
--   [ 12.0, 15.0, 18.0,
--     12.0, 15.0, 18.0,
--     12.0, 15.0, 18.0]
matSumCol :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
matSumCol r mat = replicate (constant (Z :. (r :: Int) :. All)) $ sum $ transpose mat

matSumCol' :: Matrix Double -> Matrix Double
matSumCol' m = run $ matSumCol n m'
  where
    n  = dim m
    m' = use m


-- | Proba computes de probability matrix: all cells divided by thee sum of its column
-- if you need get the probability on the lines, just transpose it
--
-- >>> run $ matProba 3 (use $ matrix 3 [1..])
-- Matrix (Z :. 3 :. 3)
--   [ 8.333333333333333e-2, 0.13333333333333333, 0.16666666666666666,
--       0.3333333333333333,  0.3333333333333333,  0.3333333333333333,
--       0.5833333333333334,  0.5333333333333333,                 0.5]
matProba :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
matProba d mat = zipWith (/) mat (matSumCol d mat)

-- | Diagonal of the matrix
--
-- >>> run $ diag (use $ matrix 3 ([1..] :: [Int]))
-- Vector (Z :. 3) [1,5,9]
diag :: Elt e
     => Acc (Matrix e)
     -> Acc (Vector e)
diag m = backpermute (indexTail (shape m))
                     (lift1 (\(Z :. x) -> (Z :. x :. (x :: Exp Int))))
                     m

-- | Divide by the Diagonal of the matrix
--
-- >>> run $ divByDiag 3 (use $ matrix 3 ([1..] :: [Double]))
-- Matrix (Z :. 3 :. 3)
--   [ 1.0, 0.4, 0.3333333333333333,
--     4.0, 1.0, 0.6666666666666666,
--     7.0, 1.6,                1.0]
divByDiag :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
divByDiag d mat = zipWith (/) mat (replicate (constant (Z :. (d :: Int) :. All)) $ diag mat)

-----------------------------------------------------------------------
-- | Filters the matrix with the minimum of maximums
--
-- >>> run $ matMiniMax $ use $ matrix 3 [1..]
-- Matrix (Z :. 3 :. 3)
--   [ 0.0, 4.0, 7.0,
--     0.0, 5.0, 8.0,
--     0.0, 6.0, 9.0]
matMiniMax :: (Elt a, Ord a, P.Num a)
           => Acc (Matrix a)
           -> Acc (Matrix a)
matMiniMax m = filterWith' miniMax' (constant 0) m
  where
    miniMax' = the $ minimum $ maximum m


-- | Filters the matrix with a constant
--
-- >>> run $ matFilter 5 $ use $ matrix 3 [1..]
-- Matrix (Z :. 3 :. 3)
--   [ 0.0, 0.0, 7.0,
--     0.0, 0.0, 8.0,
--     0.0, 6.0, 9.0]
filter' :: Double -> Acc (Matrix Double) -> Acc (Matrix Double)
filter' t m = filterWith t 0 m

filterWith :: Double -> Double -> Acc (Matrix Double) -> Acc (Matrix Double)
filterWith t v m = map (\x -> ifThenElse (x > (constant t)) x (constant v)) (transpose m)

filterWith' :: (Elt a, Ord a) => Exp a -> Exp a -> Acc (Matrix a) -> Acc (Matrix a)
filterWith' t v m = map (\x -> ifThenElse (x > t) x v) m


------------------------------------------------------------------------
------------------------------------------------------------------------



-- | TODO use Lenses
data Direction = MatCol (Exp Int) | MatRow (Exp Int) | Diag

nullOf :: Num a => Dim -> Direction -> Acc (Matrix a)
nullOf n' dir =
        let ones   = fill (index2 n n) 1
            zeros  = fill (index2 n n) 0
            n = constant n'
        in
        permute const ones ( lift1 ( \(Z :. (i :: Exp Int) :. (_j:: Exp Int))
                                                -> case dir of 
                                                     MatCol m -> (Z :. i :. m)
                                                     MatRow m -> (Z :. m :. i)
                                                     Diag     -> (Z :. i :. i)
                                   )
                           )
                           zeros

nullOfWithDiag :: Num a => Dim -> Direction -> Acc (Matrix a)
nullOfWithDiag n dir = zipWith (*) (nullOf n dir) (nullOf n Diag)


divide :: (Elt a, Ord a, P.Fractional (Exp a), P.Num a)
    => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
divide = zipWith divide'
  where
    divide' a b = ifThenElse (b > (constant 0))
                             (a / b)
                             (constant 0)

-- | Nominator
sumRowMin :: (Num a, Ord a) => Dim -> Acc (Matrix a) -> Acc (Matrix a)
sumRowMin n m = {-trace (P.show $ run m') $-} m'
  where
    m' = reshape (shape m) vs
    vs = P.foldl1 (++)
       $ P.map (\z -> sumRowMin1 n (constant z) m) [0..n-1]

sumRowMin1 :: (Num a, Ord a) => Dim -> Exp Int -> Acc (Matrix a) -> Acc (Vector a)
sumRowMin1 n x m = trace (P.show (run m,run $ transpose m)) $ m''
  where
    m'' = sum $ zipWith min (transpose m) m
    _m'  = zipWith (*) (zipWith (*) (nullOf n (MatCol x)) $ nullOfWithDiag n (MatRow x)) m

-- | Denominator
sumColMin :: (Num a, Ord a) => Dim -> Acc (Matrix a) -> Acc (Matrix a)
sumColMin n m = reshape (shape m) vs
  where
    vs = P.foldl1 (++)
       $ P.map (\z -> sumColMin1 n (constant z) m) [0..n-1]


sumColMin1 :: (Num a) => Dim -> Exp Int -> Acc (Matrix a) -> Acc (Matrix a)
sumColMin1 n x m = zipWith (*) (nullOfWithDiag n (MatCol x)) m



{- | WIP fun with indexes
selfMatrix :: Num a => Dim -> Acc (Matrix a)
selfMatrix n' =
        let zeros = fill (index2 n n) 0
            ones  = fill (index2 n n) 1
            n = constant n'
        in
        permute const ones ( lift1 ( \(Z :. (i :: Exp Int) :. (_j:: Exp Int))
                                                -> -- ifThenElse (i /= j)
                                                     --         (Z :. i :. j)
                                                              (Z :. i :. i)
                                    )) zeros

selfMatrix' :: (Elt a, P.Num (Exp a)) => Array DIM2 a -> Matrix a
selfMatrix' m' = run $ selfMatrix n
  where
    n = dim m'
    m = use m'
-}
-------------------------------------------------
-------------------------------------------------
crossProduct :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
crossProduct n m = {-trace (P.show (run m',run m'')) $-} zipWith (*) m' m''
  where
    m'  = cross n m
    m'' = transpose $ cross n m


crossT :: Matrix Double -> Matrix Double
crossT  = run . transpose . use

crossProduct' :: Matrix Double -> Matrix Double
crossProduct' m = run $ crossProduct n m'
  where
    n  = dim m
    m' = use m

runWith :: (Arrays c, Elt a1)
        => (Dim -> Acc (Matrix a1) -> a2 -> Acc c)
        -> Matrix a1
        -> a2
        -> c
runWith f m = run . f (dim m) (use m)

-- | cross
cross :: Dim -> Acc (Matrix Double) -> Acc (Matrix Double)
cross n mat = diagNull n (matSumCol n $ diagNull n mat)

cross' :: Matrix Double -> Matrix Double
cross' mat = run $ cross n mat'
  where
    mat' = use mat
    n    = dim mat


{-
-- | Hypothesis to test maybe later (or not)
-- TODO ask accelerate for instances to ease such writtings:
p_ :: (Elt e, P.Fractional (Exp e)) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
p_ m = zipWith (/) m (n_ m)
  where
    n_ :: Elt e => Acc (SymetricMatrix e) -> Acc (Matrix e)
    n_ m = backpermute (shape m)
                         (lift1 ( \(Z :. (i :: Exp Int) :. (j:: Exp Int))
                                   -> (ifThenElse (i < j) (lift (Z :. j :. j)) (lift (Z :. i :. i)) :: Exp DIM2)
                                )
                         ) m
-}

theMatrix' :: Int -> Matrix Double
theMatrix' n = run $ map fromIntegral (use $ theMatrix n)

theMatrix :: Int -> Matrix Int
theMatrix n = matrix n (dataMatrix n)
  where
    dataMatrix :: Int -> [Int]
    dataMatrix x | (P.==) x 2 = [ 1, 1
                                , 1, 2
                                ]

                 | (P.==) x 3 =  [ 1, 1, 2
                                 , 1, 2, 3
                                 , 2, 3, 4
                                 ]
                 | (P.==) x 4 =  [ 1, 1, 2, 3
                                 , 1, 2, 3, 4
                                 , 2, 3, 4, 5
                                 , 3, 4, 5, 6
                                 ]
                 | P.otherwise = P.undefined

{-
theResult :: Int -> Matrix Double
theResult n | (P.==) n 2 = let r = 1.6094379124341003 in [ 0, r, r, 0]
          | P.otherwise = [ 1, 1 ]
-}


colMatrix :: Elt e
          => Int -> [e] -> Acc (Array ((Z :. Int) :. Int) e)
colMatrix n ns = replicate (constant (Z :. (n :: Int) :. All)) v
  where
    v = use $ vector (P.length ns) ns

-----------------------------------------------------------------------

