{-|
Module      : Gargantext.Graph.Distances.Conditional
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Strict            #-}
module Gargantext.Core.Viz.Graph.Distances.Conditional
  where

import Data.Matrix hiding (identity)

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.Vector as V

import Gargantext.Prelude
import Gargantext.Core.Viz.Graph.Utils

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Optimisation issue

toBeOptimized :: (Num a, Fractional a, Ord a) => Matrix a -> Matrix a
toBeOptimized m = proba Col m

------------------------------------------------------------------------
-- | Main Functions
-- Compute the probability from axis
-- x' = x / (sum Col x)
proba :: (Num a, Fractional a) => Axis -> Matrix a -> Matrix a
proba a m = mapOn a (\c x -> x / V.sum (axis a c m)) m


mapOn :: Axis -> (AxisId -> a -> a) -> Matrix a -> Matrix a
mapOn a f m = V.foldl' f'  m (V.enumFromTo 1 (nOf a m))
  where
    f' m' c = mapOnly a f c m'

mapOnly :: Axis -> (AxisId -> a -> a) -> AxisId -> Matrix a -> Matrix a
mapOnly Col = mapCol
mapOnly Row = mapRow

mapAll :: (a -> a) -> Matrix a -> Matrix a
mapAll f m = mapOn Col (\_ -> f) m


---------------------------------------------------------------
-- | Compute a distance from axis
-- xs = (sum Col x') - x'
distFromSum :: (Num a, Fractional a)
         => Axis -> Matrix a -> Matrix a
distFromSum  a m = mapOn a (\c x -> V.sum (axis a c m) - x) m
---------------------------------------------------------------
---------------------------------------------------------------
-- | To compute included/excluded or specific/generic scores
opWith  :: (Fractional a1, Num a1)
         => (Matrix a2 -> t -> Matrix a1) -> Matrix a2 -> t -> Matrix a1
opWith op xs ys = mapAll (\x -> x / (2*n -1)) (xs `op` ys)
  where
    n = fromIntegral $ nOf Col xs
---------------------------------------------------------------


-------------------------------------------------------
conditional :: (Num a, Fractional a, Ord a) => Matrix a -> Matrix a
conditional m = filterMat (threshold m') m'
  where
------------------------------------------------------------------------
    -- | Main Operations
    -- x' = x / (sum Col x)
    x' = proba Col m

------------------------------------------------------------------------
    -- xs = (sum Col x') - x'
    xs = distFromSum Col x'
    -- ys = (sum Row x') - x'
    ys = distFromSum Row x'

------------------------------------------------------------------------
--  | Top included or excluded
    ie = opWith (+) xs ys
--  ie = ( xs + ys) / (2 * (x.shape[0] - 1))

--  | Top specific or generic
    sg = opWith (-) xs ys
--  sg = ( xs - ys) / (2 * (x.shape[0] - 1))

    nodes_kept :: [Int]
    nodes_kept = take k' $ S.toList
                         $ foldl' (\s (n1,n2) -> insert [n1,n2] s) S.empty
                         $ map fst
                         $ nodes_included k <> nodes_specific k

    nodes_included n = take n $ sortOn snd $ toListsWithIndex ie
    nodes_specific n = take n $ sortOn snd $ toListsWithIndex sg
    insert as s = foldl' (\s' a -> S.insert a s') s as
    k' = 2*k
    k = 10

    dico_nodes :: Map Int Int
    dico_nodes     = M.fromList $ zip ([1..] :: [Int]) nodes_kept
    --dico_nodes_rev = M.fromList $ zip nodes_kept [1..]

    m' = matrix (length nodes_kept) 
                (length nodes_kept) 
                (\(i,j) -> getElem ((M.!) dico_nodes i) ((M.!) dico_nodes j) x')

    threshold m'' = V.minimum
                  $ V.map (\cId -> V.maximum $ getCol cId m'')
                          (V.enumFromTo 1 (nOf Col m'')      )

    filterMat t m''  = mapAll (\x -> filter' t x) m''
      where
        filter' t' x = case (x >= t') of
                        True  -> x
                        False -> 0
------------------------------------------------------------------------
