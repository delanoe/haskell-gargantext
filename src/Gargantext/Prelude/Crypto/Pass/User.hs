{-|
Module      : Gargantext.Prelude.Crypto.Pass.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : Public Domain
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

1) quick password generator for first invitations
2) Easy password manager for User (easy to memorize) (needs list of words)

-}


module Gargantext.Prelude.Crypto.Pass.User
      where


-- 1) Quick password generator imports
import Data.Text (Text)
import Data.String (String)
import Control.Monad
import Control.Monad.Random
import qualified Data.List as List

-- 2) Easy password manager imports
import Gargantext.Prelude
import Gargantext.Prelude.Utils (shuffle)


-- 1) Quick password generator
-- Inspired by Rosetta code
-- https://www.rosettacode.org/wiki/Password_generator#Haskell
gargPass :: MonadRandom m => m Text
gargPass = cs <$> gargPass' chars 33
  where
    chars = zipWith (List.\\) charSets visualySimilar

    charSets = [ ['a'..'z']
               , ['A'..'Z']
               , ['0'..'9']
               , "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"
               ]
 
    visualySimilar = ["l","IOSZ","012","!|.,'\""]

gargPass' :: MonadRandom m => [String] -> Int -> m String
gargPass' charSets n = do
  parts <- getPartition n
  chars <- zipWithM replicateM parts (uniform <$> charSets)
  shuffle' (List.concat chars)
  where
    getPartition n' = adjust <$> replicateM (k-1) (getRandomR (1, n' `div` k))
    k = length charSets
    adjust p = (n - sum p) : p
 
shuffle' :: (Eq a, MonadRandom m) => [a] -> m [a]
shuffle' [] = pure []
shuffle' lst = do
  x <- uniform lst
  xs <- shuffle (List.delete x lst)
  return (x : xs)



-- | 2) Easy password manager
-- TODO add this as parameter to gargantext.ini
gargPassUserEasy :: (Num a, Enum a, Integral a) => a -> [b] -> IO [b]
gargPassUserEasy n = gargPassUserEasy' (100 * fromIntegral n) n

gargPassUserEasy' :: (Num a, Enum a) => Int -> a -> [b] -> IO [b]
gargPassUserEasy' threshold size wlist
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
  pure (list List.!! index)

