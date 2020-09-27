{-|
Module      : Gargantext.Core.Viz.Graph.Utils
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

These functions are used for Vector.Matrix only.

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Strict            #-}

module Gargantext.Core.Viz.Graph.Utils
  where

import Data.Matrix hiding (identity)

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.List   as L
import Gargantext.Prelude

------------------------------------------------------------------------
-- | Some utils to build the matrix from cooccurrence results

-- | For tests only, to be removed
-- m1 :: Matrix Double
-- m1 = fromList 300 300 [1..]
------------------------------------------------------------------------
------------------------------------------------------------------------
data Axis = Col | Row
------------------------------------------------------------------------
-- | Matrix functions
type AxisId = Int

-- Data.Vector.Additions
dropAt :: Int -> Vector a -> Vector a
dropAt n v = debut <> (V.tail fin)
  where
    debut = V.take n v
    fin   = V.drop n v

total :: Num a => Matrix a -> a
total m = V.sum $ V.map (\c -> V.sum (getCol c m)) (V.enumFromTo 1 (nOf Col m))

nOf :: Axis -> Matrix a -> Int
nOf Row = nrows
nOf Col = ncols

axis :: Axis -> AxisId -> Matrix a -> Vector a
axis Col = getCol
axis Row = getRow


toListsWithIndex :: Matrix a ->  [((Int, Int), a)]
toListsWithIndex m = concat' $ zip [1..] $ map (\c -> zip [1..] c) $ toLists m
  where
    concat' :: [(Int, [(Int, a)])] -> [((Int, Int), a)]
    concat' xs = L.concat $ map (\(x, ys) -> map (\(y, a) -> ((x,y), a)) ys ) xs






