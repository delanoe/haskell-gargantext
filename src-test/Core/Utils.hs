{-|
Module      : Core.Utils
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Core.Utils where

import Test.Hspec

import Gargantext.Prelude

import Gargantext.Core.Utils

-- | Core.Utils tests
test :: IO ()
test = hspec $ do
  describe "check if groupWithCounts works" $ do
    it "simple integer array" $ do
      (groupWithCounts [1, 2, 3, 1, 2, 3]) `shouldBe` [(1, 2), (2, 2), (3, 2)]

    it "string" $ do
      (groupWithCounts "abccba") `shouldBe` [('a', 2), ('b', 2), ('c', 2)]
