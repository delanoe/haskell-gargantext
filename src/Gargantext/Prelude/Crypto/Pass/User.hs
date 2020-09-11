{-|
Module      : Gargantext.Prelude.Crypto.Pass.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : Public Domain
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Prelude.Crypto.Pass.User
      where

import Data.List ((!!))
import Gargantext.Prelude
import System.Random

getRandomIndex :: Foldable t => t a -> IO Int
getRandomIndex list = randomRIO (0, (length list - 1))

getRandomElement :: [b] -> IO b
getRandomElement list = do
  index <- (getRandomIndex list)
  pure (list !! index)

generatePassword :: (Num a, Enum a) => a -> [b] -> IO [b]
generatePassword size wlist = mapM (\_ -> getRandomElement wlist) [1..size]

