{-|
Module      : Gargantext.Text
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Text gathers terms in unit of contexts.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text
  where

import qualified Data.Text as DT
--import Data.Text.IO (readFile)


import Data.Text (Text, split)

import NLP.FullStop (segment)
-----------------------------------------------------------------

import Gargantext.Core.Types
import Gargantext.Prelude hiding (filter)
-----------------------------------------------------------------

data Group = Group { _group_label  :: Terms
                   , _group_terms :: Terms
                   } deriving (Show)


clean :: Text -> Text
clean txt = DT.map clean' txt
  where
    clean' '’' = '\''
    clean' c  = c


--noApax :: Ord a => Map a Occ -> Map a Occ
--noApax m = M.filter (>1) m

-------------------------------------------------------------------
-- Contexts of text
sentences :: Text -> [Text]
sentences txt = map DT.pack $ segment $ DT.unpack txt

sentences' :: Text -> [Text]
sentences' txt = split isStop txt

isStop :: Char -> Bool
isStop c = c `elem` ['.','?','!']

unsentences :: [Text] -> Text
unsentences txts = DT.intercalate " " txts

-- | https://en.wikipedia.org/wiki/Text_mining
testText_en :: Text
testText_en = DT.pack "Text mining, also referred to as text data mining, roughly equivalent to text analytics, is the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning. Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interestingness. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities). Text analysis involves information retrieval, lexical analysis to study word frequency distributions, pattern recognition, tagging/annotation, information extraction, data mining techniques including link and association analysis, visualization, and predictive analytics. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods. A typical application is to scan a set of documents written in a natural language and either model the document set for predictive classification purposes or populate a database or search index with the information extracted."

-- | https://fr.wikipedia.org/wiki/Fouille_de_textes
testText_fr :: Text
testText_fr = DT.pack "La fouille de textes ou « l'extraction de connaissances » dans les textes est une spécialisation de la fouille de données et fait partie du domaine de l'intelligence artificielle. Cette technique est souvent désignée sous l'anglicisme text mining. Elle désigne un ensemble de traitements informatiques consistant à extraire des connaissances selon un critère de nouveauté ou de similarité dans des textes produits par des humains pour des humains. Dans la pratique, cela revient à mettre en algorithme un modèle simplifié des théories linguistiques dans des systèmes informatiques d'apprentissage et de statistiques. Les disciplines impliquées sont donc la linguistique calculatoire, l'ingénierie des langues, l'apprentissage artificiel, les statistiques et l'informatique."

-- | Ngrams Test
-- >>> ngramsTest testText
-- 248
--ngramsTest :: Text -> Int
--ngramsTest x =  length ws
--  where
--    --txt = concat <$> lines <$> clean <$> readFile filePath
--    txt = clean x
--    -- | Number of sentences
--    --ls   = sentences $ txt
--    -- | Number of monograms used in the full text
--    ws   = ngrams    $ txt
--    -- | stem ngrams
    -- TODO
    -- group ngrams
    --ocs  = occ       $ ws


