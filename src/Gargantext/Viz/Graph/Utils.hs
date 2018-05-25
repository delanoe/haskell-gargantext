{-|
Module      : Gargantext.Graph.Distances.Utils
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Strict            #-}

module Gargantext.Viz.Graph.Utils
  where

import Data.Matrix hiding (identity)

import Data.Map (Map)
import qualified Data.Map    as M

import Data.Set (Set)
import qualified Data.Set    as S

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.List   as L
import Gargantext.Prelude

------------------------------------------------------------------------
-- | Some utils to build the matrix from cooccurrence results

type Distance = Double
type Cooc     = Int
type NgramId  = Int
type Index    = Int

-- Type Families
--type Matrix' Index a
--type Matrix' NgramId a

data Matrice a = Matrice { matrice_fromIndex :: !(Map Index NgramId)
                         , matrice_toIndex   :: !(Map NgramId Index)
                         , matrice           :: !(Matrix a)
                         } deriving (Show)

--fromMatrice :: Matrice Double -> [(NgramId, NgramId, Double)]
--fromMatrice m = undefined


toMatrice :: [(NgramId, NgramId, Int)] -> Matrice Double
toMatrice ns = Matrice fromIndx toIndx m
  where
    s   = cooc2set ns
    (fromIndx, toIndx) = set2indexes s
    n   = (length (S.toList s))
    idx = toIndex toIndx ns
    m   = matrix n n (\x -> maybe 0 identity (fromIntegral <$> M.lookup x idx))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
toIndex :: Map NgramId Index -> [(NgramId, NgramId, a)] -> Map (Index,Index) a
toIndex ni ns = to ni ns

fromIndex :: Map Index NgramId -> [(Index, Index, a)] -> Map (NgramId,NgramId) a
fromIndex ni ns = to ni ns
-------------------------------------------------------------------------------
to :: (Ord b, Ord k) => Map k b -> [(k, k, a)] -> Map (b, b) a
to index ns = M.fromList $ map (\(a1,a2,c) -> ( ( (M.!) index a1
                                                , (M.!) index a2
                                                )
                                              , c 
                                              )
                                 ) ns

-------------------------------------------------------------------------------
cooc2set :: [(NgramId, NgramId, a)] -> Set NgramId
cooc2set cs' = foldl' (\s (a1,a2,_) -> insert [a1,a2] s ) S.empty cs'
  where
    insert as s = foldl' (\s' a -> S.insert a s') s as


set2indexes :: Set NgramId -> (Map Index NgramId, Map NgramId Index)
set2indexes s = (M.fromList fromIndex', M.fromList toIndex')
  where
    s' = S.toList s
    fromIndex' = zip [1..] s'
    toIndex'   = zip  s'  [1..]


------------------------------------------------------------------------
-- Data.Vector.Additions
dropAt :: Int -> Vector a -> Vector a
dropAt n v = debut <> (V.tail fin)
  where
    debut = V.take n v
    fin   = V.drop n v

------------------------------------------------------------------------
data Axis = Col | Row
---- | Matrix Algebra
--data Algebra a = Point a | Vector a | Matrix a
--
--multiply :: Algebra a -> Matrix a -> Matrix a
--multiply (Point  a) = undefined
--multiply (Vector a) = undefined
--multiply (Matrix a) = undefined
--
--div :: Fractional a => Matrix a -> Matrix a
--div m = foldl' (\m c -> divCol c m) m [1.. (ncols m)]
--  where
--    divCol c m = mapCol (\_ x -> 1/x) c m
--
--divide :: Fractional a => Matrix a -> Matrix a -> Matrix a
--divide a b = a `multStd` (div b)

------------------------------------------------------------------------
-- | Matrix functions
type AxisId = Int

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


-- | For tests only, to be removed
m1 :: Matrix Double
m1 = fromList 300 300 [1..]

