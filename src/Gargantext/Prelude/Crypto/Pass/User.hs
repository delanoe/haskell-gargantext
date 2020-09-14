{-|
Module      : Gargantext.Prelude.Crypto.Pass.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : Public Domain
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Easy password manager for User (easy to memorize).

-}


module Gargantext.Prelude.Crypto.Pass.User
      where

import Data.List ((!!))
import Gargantext.Prelude
import Gargantext.Prelude.Utils (shuffle)
import System.Random

-- TODO add this as parameter to gargantext.ini
gargPassUser :: (Num a, Enum a, Integral a) => a -> [b] -> IO [b]
gargPassUser n = gargPassUser' (100 * fromIntegral n) n

gargPassUser' :: (Num a, Enum a) => Int -> a -> [b] -> IO [b]
gargPassUser' threshold size wlist
  | length wlist > threshold  = generatePassword size wlist
  | otherwise                 = panic "List to short"

generatePassword :: (Num a, Enum a) => a -> [b] -> IO [b]
generatePassword size wlist = shuffle wlist
  >>= \wlist' -> mapM (\_ -> getRandomElement wlist') [1..size]

getRandomIndex :: Foldable t => t a -> IO Int
getRandomIndex list = randomRIO (0, (length list - 1))

getRandomElement :: [b] -> IO b
getRandomElement list = do
  index <- (getRandomIndex list)
  pure (list !! index)

