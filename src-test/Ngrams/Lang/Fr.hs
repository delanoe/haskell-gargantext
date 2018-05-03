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

module Ngrams.Lang.Fr where

import Test.Hspec

import Gargantext.Prelude
import Gargantext.Types.Main (Language(..))
import Gargantext.Text.Parser (extractNgrams, selectNgrams)

ngramsExtractionTest :: IO ()
ngramsExtractionTest = hspec $ do
    describe "Behavioral tests: ngrams extraction in French Language" $ do
        it "Groupe : adjectif et nom commun" $ do
            let textFr = "Le beau texte fut écrit."
            testFr <- map (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("beau texte","NC","O")]]

        it "Groupe : adjectifs et nom commun" $ do
            let textFr = "Le beau petit texte fut écrit."
            testFr <- map (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("beau petit texte","NC","O")]]
                -- `shouldBe` [[("beau texte","NC","O"),("petit texte","NC","O")]] ?

        it "Groupe : nom commun et adjectif" $ do
            let textFr = "Le livre blanc fut écrit."
            testFr <- map (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("livre blanc","NC","O")]]
        
        it "Groupe : nom commun et adjectifs avec conjonction" $ do
            let textFr = "Le livre blanc et rouge."
            testFr <- map (selectNgrams FR) <$> (extractNgrams FR) textFr
            testFr `shouldBe` [[("livre blanc","NC","O"),("livre rouge","NC","O")]]
                -- `shouldBe` [[("livre blanc et rouge","N","O")]] ?

        it "Groupe: Nom commun + préposition + Nom commun" $ do
            let textFr0 = "Le problème du jour est résolu."
            testFr0 <- map (selectNgrams FR) <$> (extractNgrams FR) textFr0
            testFr0 `shouldBe` [[("problème du jour","NC","O")]]

        it "Groupe: Nom commun + préposition + déterminant + Nom commun" $ do
            let textFr0 = "Emmanuel Macron est le président de la France."
            testFr0 <- map (selectNgrams FR) <$> (extractNgrams FR) textFr0
            testFr0 `shouldBe` [[("Emmanuel Macron","NPP","PERSON"),("président de la France","NC","LOCATION")]]


        it "Groupe: Nom commun + préposition + Nom commun + prép + Nom commun" $ do
            let textFr1 = "L'heure d'arrivée des coureurs dépend de la météo du jour."
            testFr1 <- map (selectNgrams FR) <$> (extractNgrams FR) textFr1
            testFr1 `shouldBe` [[("heure d' arrivée des coureurs","NC","O"),("météo du jour","NC","O")]]

