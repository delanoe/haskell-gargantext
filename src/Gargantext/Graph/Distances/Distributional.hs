{-|
Module      : Gargantext.Graph.Distances.Distributional
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Strict            #-}


module Gargantext.Graph.Distances.Distributional
  where

import Data.Matrix hiding (identity)
import Data.String.Conversions (ConvertibleStrings(..))

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Vector (Vector)
import qualified Data.Vector as V

import Gargantext.Prelude
import Gargantext.Graph.Utils


distributional :: (Floating a, Ord a) => Matrix a -> [((Int, Int), a)]
distributional m = filter (\((x,y), d) -> foldl' (&&) True (conditions x y d) ) distriList
  where
    conditions x y d  =  [ (x /= y)
                         , (d > miniMax')
                         , ((M.lookup (x,y) distriMap) > (M.lookup (y,x) distriMap))
                         ]
    distriList   = toListsWithIndex distriMatrix
    distriMatrix = ri (mi m)
    
    distriMap    = M.fromList $ distriList
    miniMax'     = miniMax distriMatrix

ri :: (Ord a, Fractional a) => Matrix a -> Matrix a
ri m = matrix c r doRi
  where
    doRi (x,y)     = doRi' x y m
    doRi' x y mi'' = sumMin x y mi'' / (V.sum $ ax Col x y mi'')

    sumMin x y mi' = V.sum $ V.map (\(a,b) -> min a b )
                           $ V.zip (ax Col x y mi') (ax Row x y mi')
    (c,r) = (nOf Col m, nOf Row m)


mi :: (Ord a, Floating a) => Matrix a -> Matrix a
mi m = matrix c r createMat
  where
    (c,r) = (nOf Col m, nOf Row m)
    createMat (x,y) = doMi x y m
    doMi x y m = if x == y then 0 else (nonNegative $ log (doMi' x y m))
    
    doMi' x y m = (getElem x y m) / ( cross x y m / total m )
    
    cross x y m = (V.sum $ ax Col x y m) * (V.sum $ ax Row x y m)
    


ax :: Axis -> Int -> Int -> Matrix a -> Vector a
ax a  i j m  = dropAt j' $ axis a i' m
                  where
                    i' = div i c + 1
                    j' = mod r j + 1
                    (c,r) = (nOf Col m, nOf Row m)

nonNegative :: (Ord a, Num a) => a -> a
nonNegative x = if x > 0 then x else 0

miniMax :: (Ord a) => Matrix a -> a
miniMax m = V.minimum $ V.map (\c -> V.maximum $ getCol c m) (V.enumFromTo 1 (nOf Col m))


