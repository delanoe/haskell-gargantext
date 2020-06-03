{-|
Module      : Gargantext.Text.Metrics.Freq
Description : Some functions to count.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Text.Metrics.Freq where

import Gargantext.Prelude
import Data.Bool (otherwise)
import Data.Map (empty, Map, insertWith, toList)
import qualified Data.List as L

countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = Data.Map.insertWith (+) e 1 m

freq :: (Ord k) => [k] -> Data.Map.Map k Int
freq = foldl countElem Data.Map.empty

getMaxFromMap :: Ord a => Map a1 a -> [a1]
getMaxFromMap m = go [] Nothing (toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest


average :: [Double] -> Double
average x = L.sum x / L.genericLength x

average' :: [Int] -> Double
average' x = (L.sum y) / (L.genericLength y) where
    y = L.map fromIntegral x


