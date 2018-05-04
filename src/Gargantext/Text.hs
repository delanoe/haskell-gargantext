{-|
Module      : Gargantext.Text
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
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text
  where

import Data.Char (Char, isAlphaNum, isSpace)
import Data.Text (Text, filter, toLower, split, splitOn)
import qualified Data.Text as DT
--import Data.Text.IO (readFile)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict  (Map
                        , empty
                        , insertWith, unionWith
                        , lookupIndex
                        --, fromList, keys
                        )
import qualified Data.Map.Strict as M (filter)
import Data.Foldable (foldl')

-----------------------------------------------------------------
import Gargantext.Text.Ngrams.Stem.En

import qualified Gargantext.Text.Metrics.FrequentItemSet as FIS
import Gargantext.Prelude hiding (filter)
-----------------------------------------------------------------


data ListName = Stop | Candidate | Graph
  deriving (Show, Eq)



data Ngroup = Ngroup { _ngroup_label  ::  Ngrams
                     , _ngroup_ngrams :: [Ngrams]
                     } deriving (Show)


data Ngrams = Ngrams { _ngrams_label :: [Text]
                     , _ngrams_stem  :: Set Text
                     } deriving (Show)

text2ngrams :: Text -> Ngrams
text2ngrams txt = Ngrams txt' (S.fromList $ map stem txt')
  where
    txt' = splitOn " " txt

equivNgrams :: Ngrams -> Ngrams -> Bool
equivNgrams (Ngrams _ s1) (Ngrams _ s2) = s1 `S.isSubsetOf` s2
                                        || s2 `S.isSubsetOf` s1


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

---- | https://en.wikipedia.org/wiki/Text_mining
--testText :: Text
--testText = DT.pack "Text mining, also referred to as text data mining, roughly equivalent to text analytics, is the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning. Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interestingness. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities). Text analysis involves information retrieval, lexical analysis to study word frequency distributions, pattern recognition, tagging/annotation, information extraction, data mining techniques including link and association analysis, visualization, and predictive analytics. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods. A typical application is to scan a set of documents written in a natural language and either model the document set for predictive classification purposes or populate a database or search index with the information extracted."
--
--
--
---- | Tests
----ngramsTest :: [Text]
--ngramsTest =  ocs
--  where
--    --txt = concat <$> lines <$> clean <$> readFile filePath
--    txt = clean $ testText
--    -- | Number of sentences
--    ls   = sentences $ txt
--    -- | Number of monograms used in the full text
--    ws   = ngrams    $ txt
--    -- | stem ngrams
--    -- TODO
--    -- group ngrams
--    ocs  = occ       $ ws
--
--
