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
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gargantext.Core.Methods.Matrix.Accelerate.Utils
import qualified Gargantext.Prelude as P

-----------------------------------------------------------------------
-- * Distributional Distance
distributional' :: Elt a => Matrix a -> Matrix a
distributional' _m' = undefined
{-
 where
    m = use m'
    n = dim m'
-}
















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

-- * For Tests (to be removed)
-- | Test perfermance with this matrix
-- TODO : add this in a benchmark folder
distriTest :: Int -> Matrix Double
distriTest n = distributional (theMatrix n)


