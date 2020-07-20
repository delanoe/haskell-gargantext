
{-|
Module      : Utils.Crypto
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Utils.Crypto where

import Data.Text (Text)
import Test.Hspec
import Gargantext.Prelude
import Gargantext.Prelude.Utils

-- | Crypto Hash tests
test :: IO ()
test = hspec $ do
  describe "Hash String with frontend works" $ do
    let text = "To hash with backend" :: Text
    let hashed = "8a69a94d164279af2b7d1443ce08da6184b3d7e815406076e148159c284b53c3" :: Hash
                 -- ^ hash from fronted with text above
    it "compare" $ do
      hash text `shouldBe` hashed

  describe "Hash List with backend works" $ do
    let list = ["a","b"] :: [Text]
    let hashed = "ab19ec537f09499b26f0f62eed7aefad46ab9f498e06a7328ce8e8ef90da6d86" :: Hash
                 -- ^ hash from frontend with text above
    it "compare" $ do
      hash list `shouldBe` hashed

------------------------------------------------------------------------
-- | TODO property based tests
  describe "Hash works with any order of list" $ do
    let hash1 = hash (["a","b"] :: [Text])
    let hash2 = hash (["b","a"] :: [Text])
    it "compare" $ do
      hash1 `shouldBe` hash2

