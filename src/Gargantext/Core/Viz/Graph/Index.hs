{-|
Module      : Gargantext.Graph.Similarities.Utils
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

import Data.Array.Accelerate (Matrix, Elt, Shape, (:.)(..), Z(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set)
import Gargantext.Prelude
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.List             as L

type Index    = Int

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
score :: (Ord t) => MatrixShape
                 -> (A.Matrix Int -> A.Matrix Double)
                 -> Map (t, t) Int
                 -> Map (t, t) Double
score s f m = fromIndex fromI . mat2map . f $ cooc2mat s toI m
  where
    (toI, fromI) = createIndices m

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cooc2mat :: Ord t => MatrixShape -> Map t Index -> Map (t, t) Int -> Matrix Int
cooc2mat sym ti m = map2mat sym 0 n idx
  where
    n = M.size ti
    idx = toIndex ti m -- it is important to make sure that toIndex is ran only once.

data MatrixShape = Triangle | Square

map2mat :: Elt a => MatrixShape -> a -> Int -> Map (Index, Index) a -> Matrix a
map2mat sym def n m = A.fromFunction shape getData
  where
    getData = (\(Z :. x :. y) ->
      case sym of
        Triangle -> fromMaybe def (M.lookup (x,y) m)
        Square   -> fromMaybe (fromMaybe def $ M.lookup (y,x) m)
                                             $ M.lookup (x,y) m
                                             )
    shape   = (Z :. n :. n)

mat2map :: (Elt a, Shape (Z :. Index)) =>
            A.Array (Z :. Index :. Index) a -> Map (Index, Index) a
mat2map m = M.fromList . map f . A.toList . A.run . A.indexed $ A.use m
  where
    -- Z :. _ :. n = A.arrayShape m
    f ((Z :. i :. j), x) = ((i, j), x)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
toIndex :: Ord t
        => Map t Index
        -> Map (t,t) a
        -> Map (Index,Index) a
toIndex = indexConversion

fromIndex :: Ord t => Map Index t -> Map (Index, Index) a -> Map (t,t) a
fromIndex ni ns = indexConversion ni ns

indexConversion :: (Ord b, Ord k) => Map k b -> Map (k,k) a -> Map (b, b) a
indexConversion index ms = M.fromList
                         $ catMaybes
                         $ map (\((k1,k2),c) -> ((,) <$> ((,) <$> M.lookup k1 index <*> M.lookup k2 index)
                                                      <*> Just c)
                                )
                         $ M.toList ms


------------------------------------------------------------------------
------------------------------------------------------------------------

--fromIndex' :: Ord t => Vector t -> Map (Index, Index) a -> Map (t,t) a
--fromIndex' vi ns = undefined

-- TODO: returning a Vector should be faster than a Map
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

------------------------------------------------------------------------
------------------------------------------------------------------------

testIndices :: Bool
testIndices = myMap == ( M.filter (>0) myMap')
  where
    xy      = L.zip ([0..30]:: [Int]) ([0..30]:: [Int])
    myMap   = M.fromList $ L.zip xy ([1..]:: [Int])
    (ti,it) = createIndices myMap
    matrix  = mat2map $ map2mat Square 0 (M.size ti) $ toIndex ti myMap
    myMap'  = fromIndex it matrix



