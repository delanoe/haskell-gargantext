{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import Data.Text (Text)
import Data.Hastext.Parsers.Occurrences (parse)

main = print "hspec $ do
  describe "Parser for occurrences" $ do
    
    let txt     = "internet"

    it "returns the result of one parsing" $ do
        parse "internet" "internet" `shouldBe` Right ((txt, 1) :: (Text, Int))

    -- | Context of Text should be toLower
    it "returns the result of one parsing not case sensitive" $ do
        let txtCase = "Internet"
        parse txtCase "internet"  `shouldBe` Right ((txtCase, 1) :: (Text, Int))

    it "returns the result of one parsing after space" $ do
        parse txt " internet"  
            `shouldBe` Right ((txt, 1) :: (Text, Int))

    it "returns the result of one parsing after chars" $ do
        parse txt "l'internet"  
            `shouldBe` (Right ((txt, 1) :: (Text, Int)))

    it "returns the result of multiple parsing" $ do
        parse txt "internet internet of things" 
            `shouldBe` (Right ((txt, 2) :: (Text, Int)))

    it "returns the result of multiple parsing separated by text" $ do
        parse txt "internet in the internet of things" 
            `shouldBe` (Right ((txt, 2) :: (Text, Int)))

    it "returns the result of multiple parsing separated by punctuation" $ do
        parse txt "internet. In the internet of things, internet like; internet?"
            `shouldBe` (Right ((txt, 4) :: (Text, Int)))



main :: IO ()

