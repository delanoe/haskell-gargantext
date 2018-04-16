{-|
Module      : Gargantext.Ngrams.FrequentItemSet
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Domain Specific Language to manage Frequent Item Set (FIS)

-}

module Gargantext.Ngrams.FrequentItemSet
  ( Fis, Size
  , occ, cooc
  , all, between
  , module HLCM
  )
  where

import Data.List (tail, filter)
import Data.Either

import HLCM

import Gargantext.Prelude

type Size = Either Int (Int, Int)


------------------------------------------------------------------------
-- | Occurrence is Frequent Item Set of size 1
occ  :: Frequency -> [[Item]] -> [Fis]
occ f is  = fisWithSize (Left 1) f is

-- | Cooccurrence is Frequent Item Set of size 2
cooc :: Frequency -> [[Item]] -> [Fis]
cooc f is = fisWithSize (Left 2) f is

all :: Frequency -> [[Item]] -> [Fis]
all f is  = fisWith Nothing f is

------------------------------------------------------------------------
between :: (Int, Int) -> Frequency -> [[Item]] -> [Fis]
between (x,y) f is = fisWithSize (Right (x,y)) f is

--maximum :: Int -> Frequency -> [[Item]] -> [Fis]
--maximum m f is = between (0,m) f is


------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Data type to type the Frequent Item Set
-- TODO replace List with Set in fisItemSet 
-- be careful : risks to erase HLCM behavior
type Fis = Fis' Item
data Fis' a = Fis' { _fisCount   :: Int
                   , _fisItemSet :: [a]
                   } deriving (Show)

-- | Sugar from items to FIS
items2fis :: [Item] -> Maybe Fis
items2fis is = case head is of
                 Nothing -> Nothing
                 Just h  -> Just (Fis' h (tail is))

------------------------------------------------------------------------
------------------------------------------------------------------------

fisWithSize :: Size -> Frequency -> [[Item]] -> [Fis]
fisWithSize n f is = case n of
  Left   n'   -> fisWith (Just (\x -> length x == (n'+1)    )) f is
  Right (a,b) -> fisWith (Just (\x -> cond1 a x && cond2 b x)) f is
                    where
                      cond1 a' x = length x >= a'
                      cond2 b' x = length x <= b'


fisWith :: Maybe ([Item] -> Bool) -> Frequency -> [[Item]] -> [Fis]
fisWith s f is = unMaybe $ map items2fis $ filter' $ runLCMmatrix is f
  where
    filter' = case s of
                Nothing  -> identity
                Just fun -> filter fun

------------------------------------------------------------------------
------------------------------------------------------------------------
