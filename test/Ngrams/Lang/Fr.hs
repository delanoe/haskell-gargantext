{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ngrams.Lang.Fr where

import Test.Hspec

import Data.Gargantext.Prelude
import Data.Gargantext.Types.Main (Language(..))
import Data.Gargantext.Ngrams.Parser (extractNgrams, selectNgrams)

ngramsExtractionTest :: IO ()
ngramsExtractionTest = hspec $ do
    describe "Behavioral tests: ngrams extraction in French Language" $ do
        it "Groupe : adjectif et nom commun" $ do
            let textFr = "Le beau texte fut écrit."
            testFr <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("beau texte","NC","O")]]

        it "Groupe : adjectifs et nom commun" $ do
            let textFr = "Le beau petit texte fut écrit."
            testFr <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("beau petit texte","NC","O")]]
                -- `shouldBe` [[("beau texte","NC","O"),("petit texte","NC","O")]] ?

        it "Groupe : nom commun et adjectif" $ do
            let textFr = "Le livre blanc fut écrit."
            testFr <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("livre blanc","NC","O")]]
        
        it "Groupe : nom commun et adjectifs avec conjonction" $ do
            let textFr = "Le livre blanc et rouge."
            testFr <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("livre blanc","N","O"),("livre rouge","N","O")]]
                -- `shouldBe` [[("livre blanc et rouge","N","O")]] ?

        it "Groupe: Nom commun + préposition + Nom commun" $ do
            let textFr0 = "Le problème du jour est résolu."
            testFr0 <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr0
            testFr0 `shouldBe` [[("problème du jour","NC","O")]]

        it "Groupe: Nom commun + préposition + Nom commun + prép + Nom commun" $ do
            let textFr1 = "L'heure d'arrivée des coureurs dépend de la météo du jour."
            testFr1 <- pm (selectNgrams FR) <$> (extractNgrams FR) textFr1
            testFr1 `shouldBe` [[("heure d' arrivée des coureurs","NC","O"),("météo du jour","NC","O")]]

