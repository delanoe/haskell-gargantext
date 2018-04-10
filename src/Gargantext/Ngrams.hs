{-|
Module      : Gargantext.Ngrams
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
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
                         , ngrams, occ, sumOcc, text2fis
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
import qualified Gargantext.Ngrams.FrequentItemSet as FIS
-----------------------------------------------------------------

import Data.Char (Char, isAlpha, isSpace)
import Data.Text (Text, words, filter, toLower)
import Data.Map.Strict  (Map, empty, keys
                        , insertWith, unionWith
                        , fromList
                        , lookupIndex
                        )
import qualified Data.Map.Strict as M (filter)
import Data.Foldable (foldl')
import Gargantext.Prelude hiding (filter)
import qualified Data.List as L (filter)

-- Maybe useful later:
--import NLP.Stemmer (stem, Stemmer(..))
--import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
--import Language.Aspell.Options (ACOption(..))

type Occ     = Int
type Index   = Int
type FreqMin = Int

ngrams :: Text -> [Text]
ngrams xs = monograms $ toLower $ filter isGram xs

monograms :: Text -> [Text]
monograms = words

isGram :: Char -> Bool
isGram '-' = True
isGram '/' = True
isGram  c  = isAlpha c || isSpace c

-- | Compute the occurrences (occ)
occ :: Ord a => [a] -> Map a Occ
occ xs = foldl' (\x y -> insertWith (+) y 1 x) empty xs

-- TODO add groups and filter stops
sumOcc :: Ord a => [Map a Occ] -> Map a Occ
sumOcc xs = foldl' (\x y -> unionWith (+) x y) empty xs

noApax :: Ord a => Map a Occ -> Map a Occ
noApax m = M.filter (>1) m

-- | /!\ indexes are not the same:

-- | Index ngrams from Map
indexNgram :: Ord a => Map a Occ -> Map Index a
indexNgram m = fromList (zip [1..] (keys m))

-- | Index ngrams from Map
ngramIndex :: Ord a => Map a Occ -> Map a Index
ngramIndex m = fromList (zip (keys m) [1..])

indexWith :: Ord a => Map a Occ -> [a] -> [Int]
indexWith m xs = unMaybe $ map (\x -> lookupIndex x m) xs

indexIt :: Ord a => [[a]] -> (Map a Int, [[Int]])
indexIt xs = (m, is)
  where
    m  = sumOcc (map occ  xs)
    is = map    (indexWith m) xs

list2fis :: Ord a => FIS.Frequency -> [[a]] -> (Map a Int, [FIS.Fis])
list2fis n xs = (m, fs)
  where
    (m, is) = indexIt xs
    fs      = FIS.all n is

text2fis :: FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
text2fis n xs = list2fis n (map ngrams xs)

text2fisWith :: FIS.Size -> FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
text2fisWith = undefined

