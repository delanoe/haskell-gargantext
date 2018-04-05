{-|
Module      : Gargantext.Ngrams
Description : Ngrams tools
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams exctration.

Definitions of ngrams.
n non negative integer

-}

module Gargantext.Ngrams ( module Gargantext.Ngrams.Letters
                              --, module Gargantext.Ngrams.Hetero
                         , module Gargantext.Ngrams.CoreNLP
                         , module Gargantext.Ngrams.Parser
                         , module Gargantext.Ngrams.Occurrences
                         , module Gargantext.Ngrams.TextMining
                         , module Gargantext.Ngrams.Metrics
                         , ngrams, occurrences
                             --, module Gargantext.Ngrams.Words
                         ) where

import Gargantext.Ngrams.Letters
--import Gargantext.Ngrams.Hetero
import Gargantext.Ngrams.CoreNLP
import Gargantext.Ngrams.Parser


import Gargantext.Ngrams.Occurrences
import Gargantext.Ngrams.TextMining
--import Gargantext.Ngrams.Words

import Gargantext.Ngrams.Metrics

-----------------------------------------------------------------

import Data.Char (Char, isAlpha, isSpace)
import Data.Text (Text, words, filter, toLower)
import Data.Map.Strict  (Map, empty, insertWith)
import Data.Foldable (foldl')
import Gargantext.Prelude hiding (filter)

-- Maybe useful later:
--import NLP.Stemmer (stem, Stemmer(..))
--import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
--import Language.Aspell.Options (ACOption(..))


ngrams :: Text -> [Text]
ngrams xs = monograms $ toLower $ filter isGram xs

monograms :: Text -> [Text]
monograms = words

isGram :: Char -> Bool
isGram '-' = True
isGram  c  = isAlpha c || isSpace c

-- | Compute the occurrences
occurrences :: Ord a => [a] -> Map a Int
occurrences xs = foldl' (\x y -> insertWith (+) y 1 x) empty xs




