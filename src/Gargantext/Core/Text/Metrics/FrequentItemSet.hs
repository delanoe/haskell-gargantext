{-|
Module      : Gargantext.Core.Text.Metrics.FrequentItemSet
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Domain Specific Language to manage Frequent Item Set (FIS)

-}


module Gargantext.Core.Text.Metrics.FrequentItemSet
  ( Fis, Size(..)
  , occ_hlcm, cooc_hlcm
  , allFis, between
  , fisWithSize
  , fisWith
  , fisWithSizePoly
  , fisWithSizePoly2
  , fisWithSizePolyMap
  , fisWithSizePolyMap'
  , module HLCM
  )
  where

import Data.List (concat, null)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isNothing)
import Data.Set (Set)
import Gargantext.Prelude
import HLCM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import Control.Monad (sequence)

data Size = Point Int | Segment Int Int

------------------------------------------------------------------------
-- | Occurrence is Frequent Item Set of size 1
occ_hlcm  :: Frequency -> [[Item]] -> [Fis]
occ_hlcm = fisWithSize (Point 1)

-- | Cooccurrence is Frequent Item Set of size 2
cooc_hlcm :: Frequency -> [[Item]] -> [Fis]
cooc_hlcm = fisWithSize (Point 2)

allFis :: Frequency -> [[Item]] -> [Fis]
allFis = fisWith Nothing

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
fisWith s f is = case filter (not . null) is of
                   [] -> []
                   js -> catMaybes $ map items2fis $ filter' $ runLCMmatrix js f
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

---- Weighted [[Item]]

isSublistOf :: Ord a => [a] -> [a] -> Bool
isSublistOf sub lst = all (\i -> elem i lst) sub

reIndexFis :: Ord a => [([a],(b,c))] -> [Fis' a] -> [(Fis' a,([b],[c]))]
reIndexFis items fis = map (\f -> 
    let docs = filter (\(lst,_) -> isSublistOf (_fisItemSet f) lst) items
     in (f, (map (fst . snd) docs, map (snd . snd) docs))) fis

wsum :: [Maybe Double] -> Int -> Maybe Double
wsum lst sup =
  let w = fmap sum $ sequence lst
   in
      if (isNothing w)
        then Just $ fromIntegral sup
        else w

fisWithSizePolyMap' :: Ord a => Size -> Frequency -> [([a], (Maybe Double,[Int]))] -> Map (Set a) (Int, (Maybe Double,[Int]))
fisWithSizePolyMap' n f is = Map.fromList
  $ map (\(fis,(ws,sources)) -> (Set.fromList (_fisItemSet fis),(_fisCount fis,(wsum ws (_fisCount fis),concat sources))))
  $ reIndexFis is 
  $ fisWithSizePoly2 n f (map fst is)

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

