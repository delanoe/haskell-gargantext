{-|
Module      : Gargantext.Graph.Distances.Utils
Description : Tools to compute distances from Cooccurrences
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Basically @compute@ takes an accelerate function as first input, a Map
of coccurrences as second input and outputs a Map automatically using
indexes.

TODO:
--cooc2fgl :: Ord t, Integral n => Map (t, t) n -> Graph
--fgl2json

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}


module Gargantext.Viz.Graph.Index
  where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A
import Data.Array.Accelerate (Matrix, Elt, Shape, (:.)(..), Z(..))

import qualified Data.Vector.Unboxed as DVU
import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map.Strict    as M

import Gargantext.Prelude

type Index    = Int


-------------------------------------------------------------------------------
{-
map'' :: (Ord t) => (A.Matrix Int -> A.Matrix Double)
                -> Map (t, t) Int
                -> Map (t, t) Double
map'' f m = back . f' . from m
  where
    from (fs, m') = unzip $ M.toAscList m
    f'  = f $ A.fromList shape m'
    shape = (A.Z A.:. n A.:. n)
    back = M.fromAscList . zip fs . A.toList
-}
-------------------------------------------------------------------------------
map' :: (Ord t) => (A.Matrix Int -> A.Matrix Double)
                -> Map (t, t) Int
                -> Map (t, t) Double
map' f m = fromIndex fromI . mat2cooc . f $ cooc2mat toI m
  where
    (toI, fromI) = createIndexes m

map'' m = cooc2mat toI m
  where
    (toI, fromI) = createIndexes m

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cooc2mat :: Ord t => Map t Index -> Map (t, t) Int -> Matrix Int
cooc2mat ti m = map2mat 0 n idx
  where
    n = M.size ti
    idx = toIndex ti m -- it is important to make sure that toIndex is ran only once.

map2mat :: Elt a => a -> Int -> Map (Index, Index) a -> Matrix a
map2mat def n m = A.fromFunction shape (\(Z :. x :. y) -> fromMaybe def $ M.lookup (x, y) m)
  where
    shape = (Z :. n :. n)

-- TODO rename mat2map
mat2cooc :: (Elt a, Shape (Z :. Index)) =>
            A.Array (Z :. Index :. Index) a -> Map (Index, Index) a
mat2cooc m = M.fromList . map f . A.toList . A.run . A.indexed $ A.use m
  where
    Z :. _ :. n = A.arrayShape m
    f ((Z :. i :. j), x) = ((i, j), x)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
toIndex :: Ord t => Map t Index -> Map (t,t) a -> Map (Index,Index) a
toIndex ni ns = indexConversion ni ns

fromIndex :: Ord t => Map Index t -> Map (Index, Index) a -> Map (t,t) a
fromIndex ni ns = indexConversion ni ns
---------------------------------------------------------------------------------
indexConversion :: (Ord b, Ord k) => Map k b -> Map (k,k) a -> Map (b, b) a
indexConversion index ms = M.fromList $ map (\((k1,k2),c) -> ( ((M.!) index k1, (M.!) index k2), c)) (M.toList ms)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
createIndexes :: Ord t => Map (t, t) b -> (Map t Index, Map Index t)
createIndexes = set2indexes . cooc2set
  where
    cooc2set :: Ord t => Map (t, t) a -> Set t
    cooc2set cs' = foldl' (\s ((t1,t2),_) -> insert [t1,t2] s ) S.empty (M.toList cs')
      where
        insert as s = foldl' (\s' t -> S.insert t s') s as

    set2indexes :: Ord t => Set t -> (Map t Index, Map Index t)
    set2indexes s = (M.fromList toIndex', M.fromList fromIndex')
      where
        fromIndex' = zip [0..] xs
        toIndex'   = zip xs [0..]
        xs         = S.toList s


