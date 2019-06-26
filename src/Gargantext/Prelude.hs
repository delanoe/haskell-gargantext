{-|
Module      : Gargantext.Prelude
Description : Specific Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

{-# LANGUAGE     NoImplicitPrelude       #-}
{-# LANGUAGE     OverloadedStrings       #-}
{-# LANGUAGE     RankNTypes              #-}

module Gargantext.Prelude
  ( module Gargantext.Prelude
  , module Protolude
  , headMay, lastMay
  , module GHC.Err.Located
  , module Text.Show
  , module Text.Read
  , cs
  , module Data.Maybe
  , round
  , sortWith
  )
  where

import GHC.Exts (sortWith)
import GHC.Err.Located (undefined)
import GHC.Real (round)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust, fromJust, maybe)
import Data.Text (Text)
import Protolude ( Bool(True, False), Int, Int64, Double, Integer
                 , Fractional, Num, Maybe(Just,Nothing)
                 , Enum, Bounded, Float
                 , Floating, Char, IO
                 , pure, (>>=), (=<<), (<*>), (<$>)
                 , putStrLn
                 , head, flip
                 , Ord, Integral, Foldable, RealFrac, Monad, filter
                 , reverse, map, mapM, zip, drop, take, zipWith
                 , sum, fromIntegral, length, fmap, foldl, foldl'
                 , takeWhile, sqrt, identity
                 , abs, min, max, maximum, minimum, return, snd, truncate
                 , (+), (*), (/), (-), (.), ($), (&), (**), (^), (<), (>), log
                 , Eq, (==), (>=), (<=), (<>), (/=)
                 , (&&), (||), not, any, all
                 , fst, snd, toS
                 , elem, die, mod, div, const, either
                 , curry, uncurry, repeat
                 , otherwise, when
                 , IO()
                 , compare
                 , on
                 , panic
                 )

-- TODO import functions optimized in Utils.Count
-- import Protolude hiding (head, last, all, any, sum, product, length)
-- import Gargantext.Utils.Count
import qualified Data.List     as L hiding (head, sum)
import qualified Control.Monad as M

import Data.Map (Map)
import qualified Data.Map as M

import Data.Map.Strict (insertWith)
import qualified Data.Vector as V
import Safe (headMay, lastMay, initMay, tailMay)
import Text.Show (Show(), show)
import Text.Read (Read())
import Data.String.Conversions (cs)


printDebug :: (Show a, MonadIO m) => [Char] -> a -> m ()
printDebug msg x = putStrLn $ msg <> " " <> show x
-- printDebug _ _ = pure ()


map2 :: (t -> b) -> [[t]] -> [[b]]
map2 fun = map (map fun)


-- Some Statistics sugar functions
-- Exponential Average
eavg :: [Double] -> Double
eavg (x:xs) = a*x + (1-a)*(eavg xs)
  where a = 0.70
eavg [] = 0

-- Simple Average
mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)


sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = fmap sum . M.sequence

variance :: Floating a => [a] -> a
variance xs = sum ys  / (fromIntegral (length xs) - 1)
  where
    m = mean xs
    ys = map (\x -> (x - m) ** 2) xs


deviation :: Floating a => [a] -> a
deviation = sqrt . variance

movingAverage :: (Eq b, Fractional b) => Int -> [b] -> [b]
movingAverage steps xs = map mean $ chunkAlong steps 1 xs

ma :: [Double] -> [Double]
ma = movingAverage 3

-- | splitEvery n == chunkAlong n n
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs =
  let (h,t) = L.splitAt n xs
  in  h : splitEvery n t

type Grain = Int
type Step  = Int

-- | Function to split a range into chunks
-- if   step == grain then linearity (splitEvery)
-- elif step < grain then overlapping
-- else dotted with holes
-- TODO FIX BUG if Steps*Grain /= length l
-- chunkAlong 10 10 [1..15] == [1..10]
-- BUG: what about the rest of (divMod 15 10)?
-- TODO: chunkAlongNoRest or chunkAlongWithRest
-- default behavior: NoRest

chunkAlong :: Eq a => Grain -> Step -> [a] -> [[a]]
chunkAlong a b l = case a >= length l of
  True  -> [l]
  False -> chunkAlong' a b l

chunkAlong' :: Eq a => Grain -> Step -> [a] -> [[a]]
chunkAlong' a b l = case a > 0 && b > 0 of
  True  -> chunkAlong'' a b l
  False -> panic "ChunkAlong: Parameters should be > 0 and Grain > Step"

chunkAlong'' :: Eq a => Int -> Int -> [a] -> [[a]]
chunkAlong'' a b l = filter (/= []) $ only (while dropAlong)
  where
    only      = map       (take a)
    while     = takeWhile (\x -> length x >= a)
    dropAlong = L.scanl   (\x _y -> drop b x) l ([1..] :: [Integer])

-- | Optimized version (Vector)
chunkAlongV :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
chunkAlongV a b l = only (while  dropAlong)
  where
    only      = V.map       (V.take a)
    while     = V.takeWhile (\x -> V.length x >= a)
    dropAlong = V.scanl     (\x _y -> V.drop b x) l (V.fromList [1..])

-- | TODO Inverse of chunk ? unchunkAlong ?
-- unchunkAlong :: Int -> Int -> [[a]] -> [a]
-- unchunkAlong = undefined


-- splitAlong [2,3,4] ("helloworld" :: [Char]) == ["he", "llo", "worl", "d"]
splitAlong :: [Int] -> [Char] -> [[Char]]
splitAlong _ [] = [] -- No list? done
splitAlong [] xs = [xs] -- No place to split at? Return the remainder
splitAlong (x:xs) ys = take x ys : splitAlong xs (drop x ys) -- take until our split spot, recurse with next split spot and list remainder

takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (a:as) = do
    v <- a
    if p v
        then do
            vs <- takeWhileM p as
            return (v:vs)
        else return []

-- SUMS
-- To select the right algorithme according to the type:
-- https://github.com/mikeizbicki/ifcxt

sumSimple :: Num a => [a] -> a
sumSimple = L.foldl' (+) 0

-- | https://en.wikipedia.org/wiki/Kahan_summation_algorithm
sumKahan :: Num a => [a] -> a
sumKahan = snd . L.foldl' go (0,0)
    where
        go (c,t) i = ((t'-t)-y,t')
            where
                y  = i-c
                t' = t+y

-- | compute part of the dict
count2map :: (Ord k, Foldable t) => t k -> Map k Double
count2map xs = M.map (/ (fromIntegral (length xs))) (count2map' xs)

-- | insert in a dict
count2map' :: (Ord k, Foldable t) => t k -> Map k Double
count2map' xs = L.foldl' (\x y -> insertWith (+) y 1 x) M.empty xs


trunc :: (RealFrac a, Integral c, Integral b) => b -> a -> c
trunc n = truncate . (* 10^n)

trunc' :: Int -> Double -> Double
trunc' n x = fromIntegral $ truncate $ (x * 10^n)


------------------------------------------------------------------------
bool2num :: Num a => Bool -> a
bool2num True  = 1
bool2num False = 0

bool2double :: Bool -> Double
bool2double = bool2num

bool2int :: Bool -> Int
bool2int = bool2num
------------------------------------------------------------------------

-- Normalizing && scaling data
scale :: [Double] -> [Double]
scale = scaleMinMax

scaleMinMax :: [Double] -> [Double]
scaleMinMax xs = map (\x -> (x - mi / (ma - mi + 1) )) xs'
    where
        ma  = maximum xs'
        mi  = minimum xs'
        xs' = map abs xs

scaleNormalize :: [Double] -> [Double]
scaleNormalize xs = map (\x -> (x - v / (m + 1))) xs'
    where
        v   = variance  xs'
        m   = mean      xs'
        xs' = map abs xs

normalize :: [Double] -> [Double]
normalize as = normalizeWith identity as

normalizeWith :: Fractional b => (a -> b) -> [a] -> [b]
normalizeWith extract bs = map (\x -> x/(sum bs')) bs'
    where
        bs' = map extract bs

-- Zip functions to add
zipFst :: ([b] -> [a]) -> [b] -> [(a, b)]
zipFst  f xs = zip (f xs) xs

zipSnd :: ([a] -> [b]) -> [a] -> [(a, b)]
zipSnd f xs = zip xs (f xs)

-- | maximumWith
maximumWith :: (Ord a1, Foldable t) => (a2 -> a1) -> t a2 -> a2
maximumWith f = L.maximumBy (compare `on` f)


-- | To get all combinations of a list with no repetition and apply a function to the resulting list of pairs
listToCombi :: forall a b. (a -> b) -> [a] -> [(b,b)]
listToCombi f l = [ (f x, f y) | (x:rest) <- L.tails l,  y <- rest ]

------------------------------------------------------------------------
-- Empty List Sugar Error Handling
-- TODO add Garg Monad Errors

listSafe1 :: Text -> ([a] -> Maybe a)
          -> Text -> [a] -> a
listSafe1 s f e xs = maybe (panic $ h <> e) identity (f xs)
  where
    h = "[ERR][Gargantext] Empty list for " <> s <> " in "

head' :: Text -> [a] -> a
head' = listSafe1 "head" headMay

last' :: Text -> [a] -> a
last' = listSafe1 "last" lastMay

------------------------------------------------------------------------

listSafeN :: Text -> ([a] -> Maybe [a])
          -> Text -> [a] -> [a]
listSafeN s f e xs = maybe (panic $ h <> e) identity (f xs)
  where
    h = "[ERR][Gargantext] Empty list for " <> s <> " in "

tail' :: Text -> [a] -> [a]
tail' = listSafeN "tail" tailMay

init' :: Text -> [a] -> [a]
init' = listSafeN "init" initMay

