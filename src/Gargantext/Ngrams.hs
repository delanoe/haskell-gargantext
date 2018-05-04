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

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Ngrams ( module Gargantext.Ngrams.Letters
                              --, module Gargantext.Ngrams.Hetero
                         , module Gargantext.Ngrams.CoreNLP
                         , module Gargantext.Ngrams.Parser
                         , module Gargantext.Ngrams.Occurrences
                         , module Gargantext.Ngrams.TextMining
                         , module Gargantext.Ngrams.Metrics
                         , Ngrams(..), ngrams, occ, sumOcc, text2fis, clean
                         , ListName(..), equivNgrams, isGram, sentences
                         , ngramsTest
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

import Data.List (sort)
import Data.Char (Char, isAlphaNum, isSpace)
import Data.Text (Text, filter, toLower, split, lines, concat)
import qualified Data.Text as DT
import Data.Text.IO (readFile)

import Data.Map.Strict  (Map
                        , empty
                        , insertWith, unionWith
                        , lookupIndex
                        --, fromList, keys
                        )
import qualified Data.Map.Strict as M (filter)
import Data.Foldable (foldl')
import Gargantext.Prelude hiding (filter)

-- Maybe useful later:
--import NLP.Stemmer (stem, Stemmer(..))
--import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
--import Language.Aspell.Options (ACOption(..))


data ListName = Stop | Candidate | Graph
  deriving (Show, Eq)

data Ngrams = Ngrams { _ngramsNgrams   :: [Text]
                     , _ngramsStem     :: [Text]
                     , _ngramsListName :: Maybe ListName
                     } deriving (Show)

equivNgrams :: Ngrams -> Ngrams -> Bool
equivNgrams  (Ngrams n1 s1 _) (Ngrams n2 s2 _)
  = (sort n1) == (sort n2) || (sort s1) == (sort s2)

type Occ     = Int
--type Index   = Int

-- Data Ngrams = Monograms | MultiGrams

ngrams :: Text -> [Text]
ngrams xs = monograms $ toLower $ filter isGram xs

clean :: Text -> Text
clean txt = DT.map clean' txt
  where
    clean' '’' = '\''
    clean' c  = c

monograms :: Text -> [Text]
monograms txt = split isWord txt
  where
    isWord c = c `elem` [' ', '\'', ',', ';']

isGram :: Char -> Bool
isGram  c  = isAlphaNum c || isSpace c || c `elem` ['-','/','\'']

-- | Compute the occurrences (occ)
occ :: Ord a => [a] -> Map a Occ
occ xs = foldl' (\x y -> insertWith (+) y 1 x) empty xs

-- TODO add groups and filter stops
sumOcc :: Ord a => [Map a Occ] -> Map a Occ
sumOcc xs = foldl' (unionWith (+)) empty xs

--noApax :: Ord a => Map a Occ -> Map a Occ
--noApax m = M.filter (>1) m

-- | /!\ indexes are not the same:

-- | Index ngrams from Map
--indexNgram :: Ord a => Map a Occ -> Map Index a
--indexNgram m = fromList (zip [1..] (keys m))

-- | Index ngrams from Map
--ngramIndex :: Ord a => Map a Occ -> Map a Index
--ngramIndex m = fromList (zip (keys m) [1..])

indexWith :: Ord a => Map a Occ -> [a] -> [Int]
indexWith m xs = unMaybe $ map (\x -> lookupIndex x m) xs

indexIt :: Ord a => [[a]] -> (Map a Int, [[Int]])
indexIt xs = (m, is)
  where
    m  = sumOcc (map occ  xs)
    is = map    (indexWith m) xs

list2fis :: Ord a => FIS.Frequency -> [[a]] -> (Map a Int, [FIS.Fis])
list2fis n xs = (m', fs)
  where
    (m, is) = indexIt xs
    m'      = M.filter (>50000) m
    fs      = FIS.all n is

text2fis :: FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
text2fis n xs = list2fis n (map ngrams xs)

--text2fisWith :: FIS.Size -> FIS.Frequency -> [Text] -> (Map Text Int, [FIS.Fis])
--text2fisWith = undefined

-------------------------------------------------------------------
-- Contexts of text

sentences :: Text -> [Text]
sentences txt = split isStop txt

isStop :: Char -> Bool
isStop c = c `elem` ['.','?','!']


-- | Tests
-- TODO http://hackage.haskell.org/package/tokenize-0.3.0/docs/NLP-Tokenize-Text.html
ngramsTest :: (IO [Text], IO [Text], IO (Map Text Occ))
ngramsTest =  (ws, ls, ocs)
  where
    txt = concat <$> lines <$> clean <$> readFile "Giono-arbres.txt"
    -- | Number of sentences
    ls   = sentences <$> txt
    -- | Number of monograms used in the full text
    ws   = ngrams    <$> txt
    -- | stem ngrams
    -- TODO
    -- group ngrams
    ocs  = occ       <$> ws




