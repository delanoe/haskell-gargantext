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
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MonoLocalBinds    #-}

module Gargantext.Core.Viz.Graph.Index
  where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A
import Data.Array.Accelerate (Matrix, Elt, Shape, (:.)(..), Z(..))

import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map.Strict    as M

-- import Data.Vector (Vector)

import Gargantext.Prelude

type Index    = Int

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
score :: (Ord t) => (A.Matrix Int -> A.Matrix Double)
                -> Map (t, t) Int
                -> Map (t, t) Double
score f m = fromIndex fromI . mat2map . f $ cooc2mat toI m
  where
    (toI, fromI) = createIndices m

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

mat2map :: (Elt a, Shape (Z :. Index)) =>
            A.Array (Z :. Index :. Index) a -> Map (Index, Index) a
mat2map m = M.fromList . map f . A.toList . A.run . A.indexed $ A.use m
  where
    -- Z :. _ :. n = A.arrayShape m
    f ((Z :. i :. j), x) = ((i, j), x)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
toIndex :: Ord t => Map t Index -> Map (t,t) a -> Map (Index,Index) a
toIndex ni ns = indexConversion ni ns

fromIndex :: Ord t => Map Index t -> Map (Index, Index) a -> Map (t,t) a
fromIndex ni ns = indexConversion ni ns

indexConversion :: (Ord b, Ord k) => Map k b -> Map (k,k) a -> Map (b, b) a
indexConversion index ms = M.fromList $ map (\((k1,k2),c) -> ( ((M.!) index k1, (M.!) index k2), c)) (M.toList ms)
---------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--fromIndex' :: Ord t => Vector t -> Map (Index, Index) a -> Map (t,t) a
--fromIndex' vi ns = undefined

-- TODO: returing a Vector should be faster than a Map
-- createIndices' :: Ord t => Map (t, t) b -> (Map t Index, Vector t)
-- createIndices' = undefined

createIndices :: Ord t => Map (t, t) b -> (Map t Index, Map Index t)
createIndices = set2indices . map2set
  where
    map2set :: Ord t => Map (t, t) a -> Set t
    map2set cs' = foldl' (\s ((t1,t2),_) -> insert [t1,t2] s ) S.empty (M.toList cs')
      where
        insert as s = foldl' (\s' t -> S.insert t s') s as

    set2indices :: Ord t => Set t -> (Map t Index, Map Index t)
    set2indices s = (M.fromList toIndex', M.fromList fromIndex')
      where
        fromIndex' = zip [0..] xs
        toIndex'   = zip xs [0..]
        xs         = S.toList s


