{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import Data.Text (Text)

import Data.Gargantext.Prelude
import Data.Gargantext.Types.Main (Language(..))
import Data.Gargantext.Ngrams
import Data.Gargantext.Ngrams.Occurrences (parseOccurrences)
import Data.Gargantext.Ngrams.Parser (extractNgrams, selectNgrams)


ngramsExtractionTest EN = hspec $ do
    describe "Ngrams extraction in English Language" $ do
        let textTest = [ "Alcoholic extract of Kaempferia galanga was tested for analgesic and antiinflammatory activities in animal models. ", "Three doses, 300 mg/kg, 600 mg/kg and 1200 mg/kg of the plant extract prepared as a suspension in 2 ml of 2% gum acacia were used. ", " Acute and sub acute inflammatory activities were studied in rats by carrageenan induced paw edema and cotton pellet induced granuloma models respectively. ", "In both models, the standard drug used was aspirin 100 mg/kg. ", "Two doses 600 mg/kg and 1200 mg/kg of plant extract exhibited significant (P<0.001) antiinflammatory activity in carrageenan model and cotton pellet granuloma model in comparison to control. ", "Analgesic activity was studied in rats using hot plate and tail-flick models. ", "Codeine 5 mg/kg and vehicle served as standard and control respectively. ", "The two doses of plant extract exhibited significant analgesic activity in tail flick model (P<0.001) and hot plate model (P<0.001) in comparison to control. ", "In conclusion K. galanga possesses antiinflammatory and analgesic activities. "] :: [String]

        it "\"Of\" seperates two ngrams" $ do
            t1 <- pm (selectNgrams EN) <$> extractNgrams EN (textTest !! 0) 
            t1 `shouldBe` [[("Alcoholic extract","NN","O"),("Kaempferia galanga","NN","O"),("analgesic activities","NN+CC","O"),("antiinflammatory activities","NN+CC","O"),("animal models","NN","O")]]

        it "Tests the conjunction of coordination in two ngrams with its adjectives" $ do
            t2 <- pm (selectNgrams EN) <$> extractNgrams EN (textTest !! 2) 
            t2 `shouldBe` [[("Acute activities","NN+CC","O"),("sub acute inflammatory activities","NN+CC","O"),("rats","NNS","O"),("carrageenan","NN","O"),("paw edema","NN","O"),("cotton pellet","NN","O"),("granuloma models","NN","O")]]
            

ngramsExtractionTest FR = hspec $ do
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
            testFr1 `shouldBe` [[("heure d' arrivée des coureurs","NC","I-ORG"),("météo du jour","NC","O")]]


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

main :: IO ()
main = do
    -- parsersTest
    -- ngramsExtractionTest EN
    ngramsExtractionTest FR

