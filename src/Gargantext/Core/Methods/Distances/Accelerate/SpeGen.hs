{-|
Module      : Gargantext.Core.Methods.Distances.Accelerate.SpeGen
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

module Gargantext.Core.Methods.Distances.Accelerate.SpeGen
  where

-- import qualified Data.Foldable as P (foldl1)
-- import Debug.Trace (trace)
import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gargantext.Core.Methods.Matrix.Accelerate.Utils
import qualified Gargantext.Prelude as P


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

