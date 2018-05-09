{-|
Module      : Gargantext.Text.Ngrams
Description : Ngrams definition and tools
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

An @n-gram@ is a contiguous sequence of n items from a given sample of
text. In Gargantext application the items are words, n is a non negative
integer.

Using Latin numerical prefixes, an n-gram of size 1 is referred to as a
"unigram"; size 2 is a "bigram" (or, less commonly, a "digram"); size
3 is a "trigram". English cardinal numbers are sometimes used, e.g.,
"four-gram", "five-gram", and so on.

Source: https://en.wikipedia.org/wiki/Ngrams

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Ngrams
  where

import Data.Char (Char, isAlphaNum, isSpace)
import Data.Text (Text, split, splitOn, pack)

import Data.Set  (Set)
import qualified Data.Set as S

import Gargantext.Prelude
import Gargantext.Core

import Gargantext.Text.Ngrams.Stem (stem)


data Ngrams = Ngrams { _ngrams_label :: [Text]
                     , _ngrams_stem  :: Set Text
                     } deriving (Show)


data Terms = MonoGrams | MultiGrams
type MonoGrams  = Text
type MultiGrams = [Text]


ngrams :: Text -> [Text]
ngrams = monograms

text2ngrams :: Lang -> Text -> Ngrams
text2ngrams lang txt = Ngrams txt' (S.fromList $ map (stem lang) txt')
  where
    txt' = splitOn (pack " ") txt


equivNgrams :: Ngrams -> Ngrams -> Bool
equivNgrams (Ngrams _ s1) (Ngrams _ s2) = s1 `S.isSubsetOf` s2
                                       || s2 `S.isSubsetOf` s1

--monograms :: Text -> [Text]
--monograms xs = monograms $ toLower $ filter isGram xs

monograms :: Text -> [Text]
monograms txt = split isWord txt
  where
    isWord c = c `elem` [' ', '\'', ',', ';']

isGram :: Char -> Bool
isGram  c  = isAlphaNum c || isSpace c || c `elem` ['-','/','\'']

