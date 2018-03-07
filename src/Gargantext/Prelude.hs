{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# LANGUAGE     NoImplicitPrelude       #-}

{-
TODO: import head impossible from Protolude: why ?
-}

module Gargantext.Prelude
  ( module Gargantext.Prelude
  , module Protolude
  , headMay
  , module Text.Show
  , module Text.Read
  )
  where

import Protolude ( Bool(True, False), Int, Double, Integer
                 , Fractional, Num, Maybe(Just,Nothing)
                 , Floating, Char, IO
                 , pure, (<$>), panic
                 , Ord, Integral, Foldable, RealFrac, Monad, filter
                 , reverse, map, zip, drop, take, zipWith
                 , sum, fromIntegral, length, fmap
                 , takeWhile, sqrt, undefined, identity
                 , abs, maximum, minimum, return, snd, truncate
                 , (+), (*), (/), (-), (.), (>=), ($), (**), (^), (<), (>), (==), (<>)
                 , (&&), (||)
                 , toS
                 )

-- TODO import functions optimized in Utils.Count
-- import Protolude hiding (head, last, all, any, sum, product, length)
-- import Gargantext.Utils.Count

import qualified Data.List     as L hiding (head, sum)
import qualified Control.Monad as M
import qualified Data.Map as Map
import Data.Map.Strict (insertWith)
import qualified Data.Vector as V
import Safe (headMay)
import Text.Show (Show(), show)
import Text.Read (Read())
--pf :: (a -> Bool) -> [a] -> [a]
--pf = filter

pr :: [a] -> [a]
pr = reverse

--pm :: (a -> b) -> [a] -> [b]
--pm = map

map2 :: (t -> b) -> [[t]] -> [[b]]
map2 fun = map (map fun)

pz :: [a] -> [b] -> [(a, b)]
pz  = zip

pd :: Int -> [a] -> [a]
pd  = drop

ptk :: Int -> [a] -> [a]
ptk = take

pzw :: (a -> b -> c) -> [a] -> [b] -> [c]
pzw = zipWith

-- Exponential Average
eavg :: [Double] -> Double
eavg (x:xs) = a*x + (1-a)*(eavg xs)
  where a = 0.70
eavg [] = 0

-- Simple Average
mean :: Fractional a => [a] -> a
mean xs = if L.null xs then 0.0
                       else sum xs / fromIntegral (length xs)

sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = fmap sum . M.sequence

variance :: Floating a => [a] -> a
variance xs = mean $ map (\x -> (x - m) ** 2) xs where
    m = mean xs

deviation :: [Double] -> Double
deviation = sqrt . variance

movingAverage :: Fractional b => Int -> [b] -> [b]
movingAverage steps xs = map mean $ chunkAlong steps 1 xs

ma :: [Double] -> [Double]
ma = movingAverage 3


-- | Function to split a range into chunks
chunkAlong :: Int -> Int -> [a] -> [[a]]
chunkAlong a b l = only (while  dropAlong)
    where
        only      = map (take a)
        while     = takeWhile  (\x -> length x >= a)
        dropAlong = L.scanl (\x _y -> drop b x) l ([1..] :: [Integer])

-- | Optimized version (Vector)
chunkAlong' :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
chunkAlong' a b l = only (while  dropAlong)
    where
        only      = V.map (V.take a)
        while     = V.takeWhile  (\x -> V.length x >= a)
        dropAlong = V.scanl (\x _y -> V.drop b x) l (V.fromList [1..])

-- | TODO Inverse of chunk ? unchunkAlong ?
unchunkAlong :: Int -> Int -> [[a]] -> [a]
unchunkAlong = undefined


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
count2map :: (Ord k, Foldable t) => t k -> Map.Map k Double
count2map xs = Map.map (/ (fromIntegral (length xs))) (count2map' xs)

-- | insert in a dict
count2map' :: (Ord k, Foldable t) => t k -> Map.Map k Double
count2map' xs = L.foldl' (\x y -> insertWith (+) y 1 x) Map.empty xs


trunc :: (RealFrac a, Integral c, Integral b) => b -> a -> c
trunc n = truncate . (* 10^n)

trunc' :: Int -> Double -> Double
trunc' n x = fromIntegral $ truncate $ (x * 10^n)


bool2int :: Num a => Bool -> a
bool2int b = case b of
                  True  -> 1
                  False -> 0

bool2double :: Bool -> Double
bool2double bool = case bool of
                  True  -> 1.0
                  False -> 0.0



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
        v = variance xs'
        m = mean     xs'
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
