{-|
Module      : Gargantext.Core.Utils.DateUtils
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


module Gargantext.Core.Utils.DateUtils where

import Gargantext.Prelude
import Data.Time (UTCTime, toGregorian, utctDay)

--
--readInt :: IO [Char] -> IO Int
--readInt = readLn
--
--readBool :: IO [Char] -> IO Bool
--readBool = readLn

utc2gregorian :: UTCTime -> (Integer, Int, Int)
utc2gregorian date = toGregorian $ utctDay date

gregorian2year :: (Integer, Int, Int) -> Integer
gregorian2year (y, _m, _d) = y

utc2year :: UTCTime -> Integer
utc2year date = gregorian2year $ utc2gregorian date

averageLength :: Fractional a => [[a1]] -> a
averageLength l = fromIntegral (sum (map length l)) / fromIntegral (length l)

--main :: IO ()
--main = do
--	c <- getCurrentTime
--	print c -- $ toYear $ toGregorian $ utctDay c

