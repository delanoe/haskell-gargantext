{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ngrams.Lang.Occurrences where

import Test.Hspec
import Control.Exception (evaluate)


import Data.Text (Text)

import Data.Gargantext.Prelude
import Data.Gargantext.Types.Main (Language(..))
import Data.Gargantext.Ngrams
import Data.Gargantext.Ngrams.Occurrences (parseOccurrences)
import Data.Gargantext.Ngrams.Parser (extractNgrams, selectNgrams)


parsersTest = hspec $ do
  describe "Parser for occurrences" $ do
    
    let txt     = "internet"

    it "returns the result of one parsing" $ do
        parseOccurrences "internet" "internet" `shouldBe` Right 1

    -- | Context of Text should be toLower
    it "returns the result of one parsing not case sensitive" $ do
        let txtCase = "Internet"
        parseOccurrences txtCase "internet"  `shouldBe` Right 1

    it "returns the result of one parsing after space" $ do
        parseOccurrences txt " internet"  
            `shouldBe` Right 1

    it "returns the result of one parsing after chars" $ do
        parseOccurrences txt "l'internet"  
            `shouldBe` Right 1

    it "returns the result of multiple parsing" $ do
        parseOccurrences txt "internet internet of things" 
            `shouldBe` Right 2

    it "returns the result of multiple parsing separated by text" $ do
        parseOccurrences txt "internet in the internet of things" 
            `shouldBe` Right 2

    it "returns the result of multiple parsing separated by punctuation" $ do
        parseOccurrences txt "internet. In the internet of things, internet like; internet?"
            `shouldBe` Right 4

--  describe "Parser for nodes" $ do
--    it "returns the result of one parsing after space" $ do
--      occOfCorpus 249509 "sciences" `shouldReturn` 7

