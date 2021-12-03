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

import Data.Map (Map)
import Data.Matrix hiding (identity)
import Data.Vector (Vector)
import qualified Data.List   as L
import qualified Data.Map    as Map
import Gargantext.Prelude
import Data.List (unzip)
import qualified Data.Vector as V
import Data.Maybe (catMaybes)
import qualified Data.Set    as Set
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

------------------------------------------------------------------------
-- Utils to manage Graphs

edgesFilter :: (Ord a, Ord b) => Map (a,a) b -> Map (a,a) b
edgesFilter m = Map.fromList $ catMaybes results
  where
    results = [ let
                  ij = Map.lookup (i,j) m
                  ji = Map.lookup (j,i) m
                  in getMax (i,j) ij ji
              | i <- keys
              , j <- keys
              , i < j
              ]
    keys    = Set.toList $ Set.fromList (x <> y)
    (x,y)   = unzip $ Map.keys m




getMax :: Ord b
       => (a,a)
       -> Maybe b
       -> Maybe b
       -> Maybe ((a,a), b)
getMax (i,j) (Just d) Nothing   = Just ((i,j), d)
getMax (i,j) Nothing (Just d)   = Just ((j,i), d)
getMax ij   (Just di) (Just dj) = if di >= dj then getMax ij (Just di) Nothing
                                              else getMax ij Nothing   (Just dj)
getMax _ _ _ = Nothing


