{-|
Module      : Ngrams.Metrics
Description : 
Copyright   : Ngrams.Metrics (c)
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP                  #-}

module Ngrams.Metrics (main) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Ratio

import Test.Hspec
import Test.QuickCheck

import Gargantext.Prelude
import Gargantext.Text.Metrics

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "levenshtein" $ do
    testSwap levenshtein
    context "with concrete examples" $ do
      testPair levenshtein "kitten"   "sitting" 3
      testPair levenshtein "cake"     "drake"   2
      testPair levenshtein "saturday" "sunday"  3
      testPair levenshtein "red"      "wax"     3
#if __GLASGOW_HASKELL__ >= 710
      testPair levenshtein "a😀c"     "abc"     1
#endif
      testPair levenshtein "lucky"    "lucky"   0
      testPair levenshtein ""         ""        0
  describe "levenshteinNorm" $ do
    testSwap levenshteinNorm
    testPair levenshteinNorm "kitten"   "sitting" (4 % 7)
    testPair levenshteinNorm "cake"     "drake"   (3 % 5)
    testPair levenshteinNorm "saturday" "sunday"  (5 % 8)
    testPair levenshteinNorm "red"      "wax"     (0 % 1)
#if __GLASGOW_HASKELL__ >= 710
    testPair levenshteinNorm "a😀c"     "abc"     (2 % 3)
#endif
    testPair levenshteinNorm "lucky"    "lucky"   (1 % 1)
    testPair levenshteinNorm ""         ""        (1 % 1)
  describe "damerauLevenshtein" $ do
    testSwap damerauLevenshtein
    testPair damerauLevenshtein "veryvery long" "very long" 4
    testPair damerauLevenshtein "thing"         "think"     1
    testPair damerauLevenshtein "nose"          "ones"      2
    testPair damerauLevenshtein "thing"         "sign"      3
    testPair damerauLevenshtein "red"           "wax"       3
#if __GLASGOW_HASKELL__ >= 710
    testPair damerauLevenshtein "a😀c"          "abc"       1
#endif
    testPair damerauLevenshtein "lucky"         "lucky"     0
    testPair damerauLevenshtein ""              ""          0
  describe "damerauLevenshteinNorm" $ do
    testSwap damerauLevenshteinNorm
    testPair damerauLevenshteinNorm "veryvery long" "very long" (9 % 13)
    testPair damerauLevenshteinNorm "thing"         "think"     (4 % 5)
    testPair damerauLevenshteinNorm "nose"          "ones"      (1 % 2)
    testPair damerauLevenshteinNorm "thing"         "sign"      (2 % 5)
    testPair damerauLevenshteinNorm "red"           "wax"       (0 % 1)
#if __GLASGOW_HASKELL__ >= 710
    testPair damerauLevenshteinNorm "a😀c"          "abc"       (2 % 3)
#endif
    testPair damerauLevenshteinNorm "lucky"         "lucky"     (1 % 1)
    testPair damerauLevenshteinNorm ""              ""          (1 % 1)
  describe "hamming" $ do
    testSwap hamming
    testPair hamming "karolin" "kathrin" (Just 3)
    testPair hamming "karolin" "kerstin" (Just 3)
    testPair hamming "1011101" "1001001" (Just 2)
    testPair hamming "2173896" "2233796" (Just 3)
    testPair hamming "toned"   "roses"   (Just 3)
    testPair hamming "red"     "wax"     (Just 3)
#if __GLASGOW_HASKELL__ >= 710
    testPair hamming "a😀c"    "abc"      (Just 1)
#endif
    testPair hamming "lucky"   "lucky"   (Just 0)
    testPair hamming ""        ""        (Just 0)
    testPair hamming "small"   "big"     Nothing
  describe "overlap" $ do
    testSwap overlap
    testPair overlap "fly"     "butterfly" (1 % 1)
    testPair overlap "night"   "nacht"     (3 % 5)
    testPair overlap "context" "contact"   (5 % 7)
    testPair overlap "red"     "wax"       (0 % 1)
#if __GLASGOW_HASKELL__ >= 710
    testPair overlap "a😀c"   "abc"   (2 % 3)
#endif
    testPair overlap "lucky" "lucky" (1 % 1)
  describe "jaccard" $ do
    testSwap jaccard
    testPair jaccard "xxx"     "xyx"     (1 % 2)
    testPair jaccard "night"   "nacht"   (3 % 7)
    testPair jaccard "context" "contact" (5 % 9)
#if __GLASGOW_HASKELL__ >= 710
    testPair overlap "a😀c"     "abc"     (2 % 3)
#endif
    testPair jaccard "lucky"   "lucky"   (1 % 1)

-- | Test that given function returns the same results when order of
-- arguments is swapped.

testSwap :: (Eq a, Show a) => (Text -> Text -> a) -> SpecWith ()
testSwap f = context "if we swap the arguments" $
  it "produces the same result" $
    property $ \a b -> f a b === f b a

-- | Create spec for given metric function applying it to two 'Text' values
-- and comparing the result with expected one.

testPair :: (Eq a, Show a)
  => (Text -> Text -> a) -- ^ Function to test
  -> Text              -- ^ First input
  -> Text              -- ^ Second input
  -> a                 -- ^ Expected result
  -> SpecWith ()
testPair f a b r = it ("‘" <> T.unpack a <> "’ and ‘" <> T.unpack b <> "’") $
  f a b `shouldBe` r
