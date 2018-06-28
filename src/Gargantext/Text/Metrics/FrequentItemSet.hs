{-|
Module      : Gargantext.Text.Metrics.FrequentItemSet
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Domain Specific Language to manage Frequent Item Set (FIS)

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Metrics.FrequentItemSet
  ( Fis, Size(..)
  , occ_hlcm, cooc_hlcm
  , all, between
  , fisWithSize
  , fisWith
  , fisWithSizePoly
  , fisWithSizePoly2
  , fisWithSizePolyMap
  , module HLCM
  )
  where

import Prelude (Functor(..)) -- TODO
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V

import Data.List (filter, concat)
import Data.Maybe (catMaybes)

import HLCM

import Gargantext.Prelude

data Size = Point Int | Segment Int Int

------------------------------------------------------------------------
-- | Occurrence is Frequent Item Set of size 1
occ_hlcm  :: Frequency -> [[Item]] -> [Fis]
occ_hlcm = fisWithSize (Point 1)

-- | Cooccurrence is Frequent Item Set of size 2
cooc_hlcm :: Frequency -> [[Item]] -> [Fis]
cooc_hlcm = fisWithSize (Point 2)

all :: Frequency -> [[Item]] -> [Fis]
all = fisWith Nothing

------------------------------------------------------------------------
between :: (Int, Int) -> Frequency -> [[Item]] -> [Fis]
between (x,y) = fisWithSize (Segment x y)

--maximum :: Int -> Frequency -> [[Item]] -> [Fis]
--maximum m = between (0,m)


------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Data type to type the Frequent Item Set
-- TODO replace List with Set in fisItemSet 
-- be careful : risks to erase HLCM behavior
type Fis = Fis' Item
data Fis' a = Fis' { _fisCount   :: Int
                   , _fisItemSet :: [a]
                   } deriving (Show)

instance Functor Fis' where
  fmap f (Fis' c is) = Fis' c (fmap f is)

-- | Sugar from items to FIS
items2fis :: [Item] -> Maybe Fis
items2fis []     = Nothing
items2fis (i:is) = Just $ Fis' i is

------------------------------------------------------------------------
------------------------------------------------------------------------

fisWithSize :: Size -> Frequency -> [[Item]] -> [Fis]
fisWithSize n f is = case n of
  Point  n'   -> fisWith (Just (\x -> length x == (n'+1) )) f is
  Segment a b -> fisWith (Just (\x -> cond a (length x) b)) f is
                    where
                      cond a' x b' = a' <= x && x <= b'


                      --- Filter on Fis and not on [Item]
fisWith :: Maybe ([Item] -> Bool) -> Frequency -> [[Item]] -> [Fis]
fisWith s f is = catMaybes $ map items2fis $ filter' $ runLCMmatrix is f
-- drop unMaybe
  where
    filter' = case s of
                Nothing  -> identity
                Just fun -> filter fun

-- Here the sole purpose to take the keys as a Set is tell we do not want
-- duplicates.
fisWithSizePoly :: Ord a => Size -> Frequency -> Set a -> [[a]] -> [Fis' a]
fisWithSizePoly n f ks = map (fmap fromItem) . fisWithSize n f . map (map toItem)
  where
    ksv = V.fromList $ Set.toList ks
    ksm = Map.fromList . flip zip [0..] $ V.toList ksv
    toItem = (ksm Map.!)
    fromItem = (ksv V.!)

fisWithSizePoly2 :: Ord a => Size -> Frequency -> [[a]] -> [Fis' a]
fisWithSizePoly2 n f is = fisWithSizePoly n f ks is
  where
    ks = Set.fromList $ concat is

fisWithSizePolyMap :: Ord a => Size -> Frequency -> [[a]] -> Map (Set a) Int
fisWithSizePolyMap n f is =
  Map.fromList $ (\i -> (Set.fromList (_fisItemSet i), _fisCount i)) <$> fisWithSizePoly2 n f is


------------------------------------------------------------------------
------------------------------------------------------------------------


--
---- | /!\ indexes are not the same:
--
---- | Index ngrams from Map
----indexNgram :: Ord a => Map a Occ -> Map Index a
----indexNgram m = fromList (zip [1..] (keys m))
--
---- | Index ngrams from Map
----ngramIndex :: Ord a => Map a Occ -> Map a Index
----ngramIndex m = fromList (zip (keys m) [1..])
--
--indexWith :: Ord a => Map a Occ -> [a] -> [Int]
--indexWith m xs = unMaybe $ map (\x -> lookupIndex x m) xs
--
--indexIt :: Ord a => [[a]] -> (Map a Int, [[Int]])
--indexIt xs = (m, is)
--  where
--    m  = sumOcc (map occ  xs)
--    is = map    (indexWith m) xs
--
--list2fis :: Ord a => FIS.Frequency -> [[a]] -> (Map a Int, [FIS.Fis])
--list2fis n xs = (m', fs)
--  where
--    (m, is) = indexIt xs
--    m'      = M.filter (>50000) m
--    fs      = FIS.all n is
--
--text2fis :: FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
--text2fis n xs = list2fis n (map terms xs)
--
----text2fisWith :: FIS.Size -> FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
----text2fisWith = undefined
--

