{-|
Module      : Gargantext.Core.Methods.Distances.Accelerate.Distributional
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


* Distributional Distance metric
__Definition :__ Distributional metric is a relative metric which depends on the
selected list, it represents structural equivalence of mutual information.

__Objective :__ We want to compute with matrices processing the similarity between term $i$ and term $j$ :
 distr(i,j)=$\frac{\Sigma_{k \neq i,j} min(\frac{n_{ik}^2}{n_{ii}n_{kk}},\frac{n_{jk}^2}{n_{jj}n_{kk}})}{\Sigma_{k \neq i}\frac{n_{ik}^2}{ n_{ii}n_{kk}}}$

where $n_{ij}$ is the cooccurrence between term $i$ and term $j$

* For a vector V=[$x_1$ ... $x_n$], we note $|V|_1=\Sigma_ix_i$
* operator : .* and ./ cell by cell multiplication and division of the matrix
* operator * is the matrix multiplication
* Matrice M=[$n_{ij}$]$_{i,j}$
* opérateur : Diag(M)=[$n_{ii}$]$_i$ (vecteur)
* Id= identity matrix
* O=[1]$_{i,j}$ (matrice one)
* D(M)=Id .* M
* O * D(M) =[$n_{jj}$]$_{i,j}$
* D(M) * O =[$n_{ii}$]$_{i,j}$
* $V_i=[0~0~0~1~0~0~0]'$ en i
* MI=(M ./ O * D(M)) .* (M / D(M) * O )
* distr(i,j)=$\frac{|min(V'_i * (MI-D(MI)),V'_j * (MI-D(MI)))|_1}{|V'_i.(MI-D(MI))|_1}$

[Specifications written by David Chavalarias on Garg v4 shared NodeWrite, team Pyremiel 2020]

-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Gargantext.Core.Methods.Distances.Accelerate.Distributional
  where

-- import qualified Data.Foldable as P (foldl1)
-- import Debug.Trace (trace)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter (run)
import Gargantext.Core.Methods.Matrix.Accelerate.Utils
import qualified Gargantext.Prelude as P

-- | `distributional m` returns the distributional distance between terms each
-- pair of terms as a matrix.  The argument m is the matrix $[n_{ij}]_{i,j}$
-- where $n_{ij}$ is the coocccurrence between term $i$ and term $j$.
--
-- ## Basic example with Matrix of size 3: 
--
-- >>> theMatrixInt 3
-- Matrix (Z :. 3 :. 3)
--   [ 7, 4, 0,
--     4, 5, 3,
--     0, 3, 4]
--
-- >>> distributional $ theMatrixInt 3
-- Matrix (Z :. 3 :. 3)
--   [ 1.0, 0.0, 0.9843749999999999,
--     0.0, 1.0,                0.0,
--     1.0, 0.0,                1.0]
--
-- ## Basic example with Matrix of size 4: 
--
-- >>> theMatrixInt 4
-- Matrix (Z :. 4 :. 4)
--   [ 4, 1, 2, 1,
--     1, 4, 0, 0,
--     2, 0, 3, 3,
--     1, 0, 3, 3]
--
-- >>> distributional $ theMatrixInt 4
-- Matrix (Z :. 4 :. 4)
--   [                  1.0,                   0.0, 0.5714285714285715, 0.8421052631578947,
--                      0.0,                   1.0,                1.0,                1.0,
--     8.333333333333333e-2,             4.6875e-2,                1.0,               0.25,
--       0.3333333333333333, 5.7692307692307696e-2,                1.0,                1.0]
--
distributional :: Matrix Int -> Matrix Double
distributional m' = run result
 where
    m = map fromIntegral $ use m'
    n = dim m'

    diag_m = diag m

    d_1 = replicate (constant (Z :. n :. All)) diag_m
    d_2 = replicate (constant (Z :. All :. n)) diag_m

    mi    = (.*) ((./) m d_1) ((./) m d_2)

    -- w = (.-) mi d_mi
    
    -- The matrix permutations is taken care of below by directly replicating
    -- the matrix mi, making the matrix w unneccessary and saving one step.
    w_1 = replicate (constant (Z :. All :. n :. All)) mi
    w_2 = replicate (constant (Z :. n :. All :. All)) mi
    w' = zipWith min w_1 w_2

    -- The matrix ii = [r_{i,j,k}]_{i,j,k} has r_(i,j,k) = 0 if k = i OR k = j 
    -- and r_(i,j,k) = 1 otherwise (i.e. k /= i AND k /= j). 
    ii = generate (constant (Z :. n :. n :. n)) 
        (lift1 (\(Z :. i :. j :. k) -> cond ((&&) ((/=) k i) ((/=) k j)) 1 0))

    z_1 = sum ((.*) w' ii)
    z_2 = sum ((.*) w_1 ii)

    result = termDivNan z_1 z_2

logDistributional :: Matrix Int -> Matrix Double
logDistributional m = run
                    $ diagNull n
                    $ matMiniMax
                    $ logDistributional' n m
  where
    n = dim m

logDistributional' :: Int -> Matrix Int -> Acc (Matrix Double)
logDistributional' n m' = result
 where
    m = map fromIntegral $ use m'

    -- Scalar. Sum of all elements of m.
    to = the $ sum (flatten m)

    -- Diagonal matrix with the diagonal of m.
    d_m = (.*) m (matrixIdentity n)

    -- Size n vector. s = [s_i]_i
    s = sum ((.-) m d_m)

    -- Matrix nxn. Vector s replicated as rows. 
    s_1 = replicate (constant (Z :. All :. n)) s
    -- Matrix nxn. Vector s replicated as columns. 
    s_2 = replicate (constant (Z :. n :. All)) s

    -- Matrix nxn. ss = [s_i * s_j]_{i,j}. Outer product of s with itself. 
    ss = (.*) s_1 s_2

    -- Matrix nxn. mi = [m_{i,j}]_{i,j} where 
    -- m_{i,j} = 0 if n_{i,j} = 0 or i = j, 
    -- m_{i,j} = log(to * n_{i,j} / s_{i,j}) otherwise.
    mi = (.*) (matrixEye n) 
        (map (lift1 (\x -> cond (x == 0) 0 (log (x * to)))) ((./) m ss))

    -- Tensor nxnxn. Matrix mi replicated along the 2nd axis.
    w_1 = replicate (constant (Z :. All :. n :. All)) mi

    -- Tensor nxnxn. Matrix mi replicated along the 1st axis.
    w_2 = replicate (constant (Z :. n :. All :. All)) mi

    -- Tensor nxnxn.
    w' = zipWith min w_1 w_2

    -- A predicate that is true when the input (i, j, k) satisfy 
    -- k /= i AND k /= j
    k_diff_i_and_j = lift1 (\(Z :. i :. j :. k) -> ((&&) ((/=) k i) ((/=) k j)))

    -- Matrix nxn. 
    sumMin = sum (condOrDefault k_diff_i_and_j 0 w')

    -- Matrix nxn. All columns are the same.
    sumM = sum (condOrDefault k_diff_i_and_j 0 w_1)

    result = termDivNan sumMin sumM


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

distributional'' :: Matrix Int -> Matrix Double
distributional'' m = -- run {- $ matMiniMax -}
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

-- * For Tests (to be removed)
-- | Test perfermance with this matrix
-- TODO : add this in a benchmark folder
distriTest :: Int -> Matrix Double
distriTest n = logDistributional (theMatrixInt n)


