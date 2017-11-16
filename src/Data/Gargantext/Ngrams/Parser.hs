{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Gargantext.Ngrams.Parser where

import Data.Gargantext.Prelude
import Data.Gargantext.NLP.CoreNLP


import Data.Gargantext.Types.Main (Language(..), Ngrams)
import qualified Data.Gargantext.Ngrams.Lang.En as En
import qualified Data.Gargantext.Ngrams.Lang.Fr as Fr


-- TODO for scientific papers: add maesures
-- TODO add the p score regex
extractNgrams :: Language -> String -> IO [[Ngrams]]
extractNgrams lang s = pm (groupNgrams lang) <$> extractNgrams' s


extractNgrams' :: String -> IO [[Ngrams]]
extractNgrams' t =  pm (pm token2text)
                <$> pm _sentenceTokens
                <$> sentences
                <$> corenlp t

-- | This function selects ngrams according to grammars specific
--   of each language.
--   In english, JJ is ADJectiv in french.
selectNgrams :: Language -> [Ngrams] -> [Ngrams]
selectNgrams EN = En.selectNgrams
selectNgrams FR = Fr.selectNgrams

-- | This function analyze and groups (or not) ngrams according to 
--   grammars specific of each language.
groupNgrams :: Language -> [Ngrams] -> [Ngrams]
groupNgrams EN = En.groupNgrams
groupNgrams FR = Fr.groupNgrams


