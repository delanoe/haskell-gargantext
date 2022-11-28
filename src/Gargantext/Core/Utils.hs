{-|
Module      : Gargantext.Utils
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Gargantext.Core.Utils (
                           -- module Gargantext.Utils.Chronos
                             module Gargantext.Core.Utils.Prefix
                           , something
                           , alphanum
                           , choices
                           , randomString
                           , groupWithCounts
                           , addTuples
                          ) where

import Data.Char (chr, ord)
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Prelude ((!!))
import System.Random (initStdGen, uniformR)

-- import Gargantext.Utils.Chronos
import Gargantext.Core.Utils.Prefix
import Gargantext.Prelude


something :: Monoid a => Maybe a -> a
something Nothing  = mempty
something (Just a) = a

alphanum :: [Char]
alphanum = (chr <$> digits) <> (chr <$> lowercase) <> (chr <$> uppercase)
  where
    digits    = [(ord '0')..(ord '9')]
    lowercase = [(ord 'a')..(ord 'z')]
    uppercase = [(ord 'A')..(ord 'Z')]

choices :: Int -> [a] -> IO [a]
choices 0 _ = pure []
choices num lst = do
  gen <- initStdGen
  let (cIdx, _) = uniformR (0, length lst - 1) gen
      c = lst !! cIdx
  choices' <- choices (num - 1) lst
  pure (c:choices')

randomString :: Int -> IO Text
randomString num = do
  str <- choices num alphanum
  pure $ pack str


-- | Given a list of items of type 'a', return list with unique items
-- (like List.nub) but tuple-d with their counts in the original list
groupWithCounts :: (Ord a, Eq a) => [a] -> [(a, Int)]
groupWithCounts = map f
                . List.group
                . List.sort
  where
    f [] = panic "[groupWithCounts] impossible"
    f ts@(t:_) = (t, length ts)

addTuples :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuples (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
