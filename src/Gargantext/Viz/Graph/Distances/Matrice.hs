{-|
Module      : Gargantext.Graph.Distances.Matrix
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

module Gargantext.Viz.Graph.Distances.Matrice
  where

import qualified Data.Foldable as P (foldl1)
import Debug.Trace (trace)
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import qualified Gargantext.Prelude as P


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
-- TODO move to Utils
runExp :: Elt e => Exp e -> e
runExp e = indexArray (run (unit e)) Z
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
matProba r mat = zipWith (/) mat (matSumCol r mat)

-- | Diagonal of the matrix
--
-- >>> run $ diag (use $ matrix 3 ([1..] :: [Int]))
-- Vector (Z :. 3) [1,5,9]
diag :: Elt e => Acc (Matrix e) -> Acc (Vector e)
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
matMiniMax :: (Elt a, Ord a, P.Num a) => Acc (Matrix a) -> Acc (Matrix a)
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




-----------------------------------------------------------------------
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
measureConditional :: Matrix Int -> Matrix Double
--measureConditional m = run (matMiniMax $ matProba (dim m) $ map fromIntegral $ use m)
measureConditional m = run $ matProba (dim m)
                           $ map fromIntegral
                           $ use m


-- *** Conditional distance (advanced)

-- | Conditional distance (advanced version)
--
-- The conditional metric P(i|j) of 2 terms @i@ and @j@, also called
-- "confidence" , is the maximum probability between @i@ and @j@ to see
-- @i@ in the same context of @j@ knowing @j@.
--
-- If N(i) (resp. N(j)) is the number of occurrences of @i@ (resp. @j@)
-- in the corpus and _[n_{ij}\] the number of its occurrences we get:
--
-- \[P_c=max(\frac{n_i}{n_{ij}},\frac{n_j}{n_{ij}} )\]
conditional' :: Matrix Int -> (Matrix InclusionExclusion, Matrix SpecificityGenericity)
conditional' m = ( run $ ie $ map fromIntegral $ use m
                 , run $ sg $ map fromIntegral $ use m
                 )
  where
    ie :: Acc (Matrix Double) -> Acc (Matrix Double)
    ie mat = map (\x -> x / (2*n-1)) $ zipWith (+) (xs mat) (ys mat)
    sg :: Acc (Matrix Double) -> Acc (Matrix Double)
    sg mat = map (\x -> x / (2*n-1)) $ zipWith (-) (xs mat) (ys mat)

    n :: Exp Double
    n = P.fromIntegral r

    r :: Dim
    r = dim m

    xs :: Acc (Matrix Double) -> Acc (Matrix Double)
    xs mat = zipWith (-) (matSumCol r $ matProba r mat) (matProba r mat)
    ys :: Acc (Matrix Double) -> Acc (Matrix Double)
    ys mat = zipWith (-) (matSumCol r $ transpose $ matProba r mat) (matProba r mat)

-----------------------------------------------------------------------
-- ** Distributional Distance

-- | Distributional Distance metric
--
-- Distributional metric is a relative metric which depends on the
-- selected list, it represents structural equivalence of mutual information.
--
-- The distributional metric P(c) of @i@ and @j@ terms is: \[
-- S_{MI} = \frac {\sum_{k \neq i,j ; MI_{ik} >0}^{} \min(MI_{ik},
-- MI_{jk})}{\sum_{k \neq i,j ; MI_{ik}>0}^{}} \]
--
-- Mutual information
-- \[S_{MI}({i},{j}) = \log(\frac{C{ij}}{E{ij}})\]
--
-- Number of cooccurrences of @i@ and @j@ in the same context of text
--                        \[C{ij}\]
--
-- The expected value of the cooccurrences @i@ and @j@ (given a map list of size @n@)
--            \[E_{ij}^{m} = \frac {S_{i} S_{j}} {N_{m}}\]
--
-- Total cooccurrences of term @i@ given a map list of size @m@
--            \[S_{i} = \sum_{j, j \neq i}^{m} S_{ij}\]
--
-- Total cooccurrences of terms given a map list of size @m@
--            \[N_{m} = \sum_{i,i \neq i}^{m} \sum_{j, j \neq j}^{m} S_{ij}\]
--
distributional :: Matrix Int -> Matrix Double
distributional m = run -- $ matMiniMax
                       $ diagNull n
                       $ rIJ n
                       $ filterWith 0 100
                       $ filter' 0
                       $ s_mi
                       $ map fromIntegral
                          {- from Int to Double -}
                       $ use m
                          {- push matrix in Accelerate type -}
  where

    _ri :: Acc (Matrix Double) -> Acc (Matrix Double)
    _ri mat = mat1 -- zipWith (/) mat1 mat2
      where
        mat1 = matSumCol n $ zipWith min (_myMin mat) (_myMin $ filterWith 0 100 $ diagNull n $ transpose mat)
        _mat2 = total mat

    _myMin :: Acc (Matrix Double) -> Acc (Matrix Double)
    _myMin = replicate (constant (Z :. n :. All)) . minimum


    -- TODO fix NaN
    -- Quali TEST: OK
    s_mi :: Acc (Matrix Double) -> Acc (Matrix Double)
    s_mi m' = zipWith (\x y -> log (x / y)) (diagNull n m')
            $ zipWith (/) (crossProduct n m') (total m')
            -- crossProduct n m'


    total :: Acc (Matrix Double) -> Acc (Matrix Double)
    total = replicate (constant (Z :. n :. n)) . sum . sum

    n :: Dim
    n = dim m

-- run $ (identityMatrix (DAA.constant (10::Int)) :: DAA.Acc (DAA.Matrix Int)) Matrix (Z :. 10 :. 10)
identityMatrix :: Num a => Exp Int -> Acc (Matrix a)
identityMatrix n =
        let zeros = fill (index2 n n) 0
            ones  = fill (index1 n)   1
        in
        permute const zeros (\(unindex1 -> i) -> index2 i i) ones


eyeMatrix :: Num a => Dim -> Acc (Matrix a)
eyeMatrix n' =
        let ones   = fill (index2 n n) 1
            zeros  = fill (index1 n)   0
            n = constant n'
        in
        permute const ones (\(unindex1 -> i) -> index2 i i) zeros

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


rIJ' :: Matrix Int -> Matrix Double
rIJ' m = run $ sumRowMin (dim m) m'
  where
    m' = (map fromIntegral $ use m)

rIJ :: (Elt a, Ord a, P.Fractional (Exp a), P.Num a)
    => Dim -> Acc (Matrix a) -> Acc (Matrix a)
rIJ n m = matMiniMax $ divide a b
  where
    a = sumRowMin n m
    b = sumColMin n m

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
diagNull :: Num a => Dim -> Acc (Matrix a) -> Acc (Matrix a)
diagNull n m = zipWith (*) m eye
  where
    eye = eyeMatrix n

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
    n = dim mat


-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- * Specificity and Genericity

{- | Metric Specificity and genericity: select terms

- let N termes and occurrences of i \[N{i}\]

- Cooccurrences of i and j \[N{ij}\]
- Probability to get i given j : \[P(i|j)=N{ij}/N{j}\]

- Genericity of i  \[Gen(i)  = \frac{\sum_{j \neq i,j} P(i|j)}{N-1}\]
- Specificity of j \[Spec(i) = \frac{\sum_{j \neq i,j} P(j|i)}{N-1}\]

- \[Inclusion (i) = Gen(i) + Spec(i)\)
- \[GenericityScore = Gen(i)- Spec(i)\]

- References: Science mapping with asymmetrical paradigmatic proximity
Jean-Philippe Cointet (CREA, TSV), David Chavalarias (CREA) (Submitted
on 15 Mar 2008), Networks and Heterogeneous Media 3, 2 (2008) 267 - 276,
arXiv:0803.2315 [cs.OH]
-}
type InclusionExclusion    = Double
type SpecificityGenericity = Double

data SquareMatrix      = SymetricMatrix | NonSymetricMatrix
type SymetricMatrix    = Matrix
type NonSymetricMatrix = Matrix


incExcSpeGen :: Matrix Int -> (Vector InclusionExclusion, Vector SpecificityGenericity)
incExcSpeGen m = (run' inclusionExclusion m, run' specificityGenericity m)
  where
    run' fun mat = run $ fun $ map fromIntegral $ use mat

    -- | Inclusion (i) = Gen(i)+Spec(i)
    inclusionExclusion :: Acc (Matrix Double) -> Acc (Vector Double)
    inclusionExclusion mat = zipWith (+) (pV mat) (pV mat)
    
    -- | Genericity score = Gen(i)- Spec(i)
    specificityGenericity :: Acc (Matrix Double) -> Acc (Vector Double)
    specificityGenericity mat = zipWith (+) (pH mat) (pH mat)
    
    -- | Gen(i)  : 1/(N-1)*Sum(j!=i, P(i|j)) : Genericity  of i
    pV :: Acc (Matrix Double) -> Acc (Vector Double)
    pV mat = map (\x -> (x-1)/(cardN-1)) $ sum $ p_ij mat
    
    -- | Spec(i) : 1/(N-1)*Sum(j!=i, P(j|i)) : Specificity of j
    pH :: Acc (Matrix Double) -> Acc (Vector Double)
    pH mat = map (\x -> (x-1)/(cardN-1)) $ sum $ p_ji mat
    
    cardN :: Exp Double
    cardN = constant (P.fromIntegral (dim m) :: Double)


-- | P(i|j) = Nij /N(jj) Probability to get i given j
--p_ij :: (Elt e, P.Fractional (Exp e)) => Acc (SymetricMatrix e) -> Acc (Matrix e)
p_ij :: (Elt e, P.Fractional (Exp e)) => Acc (Matrix e) -> Acc (Matrix e)
p_ij m = zipWith (/) m (n_jj m)
  where
    n_jj :: Elt e => Acc (SymetricMatrix e) -> Acc (Matrix e)
    n_jj myMat' = backpermute (shape m)
                         (lift1 ( \(Z :. (_ :: Exp Int) :. (j:: Exp Int))
                                   -> (Z :. j :. j)
                                )
                         ) myMat'

-- | P(j|i) = Nij /N(ii) Probability to get i given j
-- to test
p_ji :: (Elt e, P.Fractional (Exp e)) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
p_ji = transpose . p_ij


-- | Step to ckeck the result in visual/qualitative tests
incExcSpeGen_proba :: Matrix Int -> Matrix Double
incExcSpeGen_proba m = run' pro m
  where
    run' fun mat = run $ fun $ map fromIntegral $ use mat

    pro mat = p_ji mat

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

-- * For Tests (to be removed)
-- | Test perfermance with this matrix
-- TODO : add this in a benchmark folder
distriTest :: Int -> Matrix Double
distriTest n = distributional (theMatrix n)

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

