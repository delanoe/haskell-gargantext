{-|
Module      : Gargantext.Core.Methods.Distances.Matrice
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

module Gargantext.Core.Methods.Distances.Matrice
  where

-- import qualified Data.Foldable as P (foldl1)
-- import Debug.Trace (trace)
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gargantext.Core.Methods.Matrix.Accelerate.Utils
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
conditional' :: Matrix Int -> (Matrix GenericityInclusion, Matrix SpecificityExclusion)
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
distributional m = -- run {- $ matMiniMax -}
                   run  $ diagNull n
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

rIJ :: (Elt a, Ord a, P.Fractional (Exp a), P.Num a)
    => Dim -> Acc (Matrix a) -> Acc (Matrix a)
rIJ n m = matMiniMax $ divide a b
  where
    a = sumRowMin n m
    b = sumColMin n m

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
type GenericityInclusion  = Double
type SpecificityExclusion = Double

data SquareMatrix      = SymetricMatrix | NonSymetricMatrix
type SymetricMatrix    = Matrix
type NonSymetricMatrix = Matrix


incExcSpeGen :: Matrix Int
             -> ( Vector GenericityInclusion
                , Vector SpecificityExclusion
                )
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
p_ji :: (Elt e, P.Fractional (Exp e))
     => Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
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

