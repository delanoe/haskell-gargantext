{-|
Module      : Ngrams.Lang
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Ngrams.Lang.En where

import Data.List ((!!))
import Data.Text (Text)

import Test.Hspec

import Gargantext.Prelude
import Gargantext.Types.Main (Language(..))
import Gargantext.Ngrams.Parser (extractNgrams, selectNgrams)


ngramsExtractionTest :: IO ()
ngramsExtractionTest = hspec $ do
    describe "Ngrams extraction in English Language" $ do
        let textTest = [ "Alcoholic extract of Kaempferia galanga was tested for analgesic and antiinflammatory activities in animal models. ", "Three doses, 300 mg/kg, 600 mg/kg and 1200 mg/kg of the plant extract prepared as a suspension in 2 ml of 2% gum acacia were used. ", " Acute and sub acute inflammatory activities were studied in rats by carrageenan induced paw edema and cotton pellet induced granuloma models respectively. ", "In both models, the standard drug used was aspirin 100 mg/kg. ", "Two doses 600 mg/kg and 1200 mg/kg of plant extract exhibited significant (P<0.001) antiinflammatory activity in carrageenan model and cotton pellet granuloma model in comparison to control. ", "Analgesic activity was studied in rats using hot plate and tail-flick models. ", "Codeine 5 mg/kg and vehicle served as standard and control respectively. ", "The two doses of plant extract exhibited significant analgesic activity in tail flick model (P<0.001) and hot plate model (P<0.001) in comparison to control. ", "In conclusion K. galanga possesses antiinflammatory and analgesic activities. "] :: [Text]

        it "\"Of\" seperates two ngrams" $ do
            t1 <- map (selectNgrams EN) <$> extractNgrams EN (textTest !! 0) 
            t1 `shouldBe` [[("Alcoholic extract of Kaempferia galanga","NN","LOCATION"),("analgesic activities","NN+CC","O"),("antiinflammatory activities","NN+CC","O"),("animal models","NN","O")]]
            
        it "Tests the conjunction of coordination in two ngrams with its adjectives" $ do
            t2 <- map (selectNgrams EN) <$> extractNgrams EN (textTest !! 2) 
            t2 `shouldBe` [[("Acute activities","NN+CC","O"),("sub acute inflammatory activities","NN+CC","O"),("rats","NNS","O"),("carrageenan","NN","O"),("paw edema","NN","O"),("cotton pellet","NN","O"),("granuloma models","NN","O")]]

        it "Tests nouns with preposition and determinants" $ do
            let t = "Donald Trump is president of the United-States of America."
            t2 <- map (selectNgrams EN) <$> extractNgrams EN t
            t2 `shouldBe` [[("Donald Trump","NNP","PERSON"),("president of the United-States of America","NN","LOCATION")]]








