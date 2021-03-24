{-|
Module      : Gargantext.Prelude.Crypto.Share
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

# Random work/research (WIP)

Goal: share secretly a sequence of random actions (either [Bool] or
[Ordering] for instances here) but without sharing secrets.

Motivation: useful to share clustering algorithm reproduction using BAC
(Ballades Aléatoires Courtes).

Question: how to certify the author of such (random) actions ? Solution
later ;)

-}

------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans        #-}
------------------------------------------------------------------------
module Gargantext.Prelude.Crypto.Share
  where

import Data.Maybe
import System.Random
import Prelude (fromEnum, toEnum)
import Gargantext.Core.Types (Ordering)
import Gargantext.Prelude

------------------------------------------------------------------------
-- | Main Types
newtype Seed = Seed Int
type Private = Seed
type Public  = Seed

------------------------------------------------------------------------
instance Random Ordering where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g


randomOrdering :: Maybe Seed -> Int -> IO [Ordering]
randomOrdering = randomWith

randomBool :: Maybe Seed -> Int -> IO [Bool]
randomBool= randomWith

------------------------------------------------------------------

randomWith :: Random a => Maybe Seed -> Int -> IO [a]
randomWith seed n = do
  g <- case seed of
    Nothing        -> newStdGen
    Just  (Seed s) -> pure $ mkStdGen s

  pure $ take n $ (randoms g)

genWith :: Private -> Public -> Int -> IO [Bool]
genWith privateSeed publicSeed n = do
  xs <- randomBool (Just  privateSeed) n
  ys <- randomBool (Just  publicSeed ) n
  pure $ zipWith xor xs ys

{-
- TODO WIP
searchSeeds :: Int -> IO [Int]
searchSeeds xs = mapM (\n -> randomWith (Just n) l) [1..]
  where
    l = length xs

shareSeed = undefined

certifySeed = undefined
-}
