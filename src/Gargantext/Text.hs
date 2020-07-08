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


module Gargantext.Text
  where

import Data.Text (Text, split)
import Gargantext.Prelude hiding (filter)
import NLP.FullStop (segment)
import qualified Data.Text as DT

-----------------------------------------------------------------

class HasText h
  where
    hasText :: h -> [Text]

-----------------------------------------------------------------
-- French words to distinguish contexts
newtype Texte      = Texte      Text
newtype Paragraphe = Paragraphe Text
newtype Phrase     = Phrase     Text
newtype MultiTerme = MultiTerme Text
newtype Mot        = Mot        Text
newtype Lettre     = Lettre     Text

-- | Type syn seems obvious
type    Titre      = Phrase

-----------------------------------------------------------------

instance Show Texte where
  show (Texte t) = show t

instance Show Paragraphe where
  show (Paragraphe p) = show p

instance Show Phrase where
  show (Phrase p) = show p

instance Show MultiTerme where
  show (MultiTerme mt) = show mt

instance Show Mot where
  show (Mot t) = show t

instance Show Lettre where
  show (Lettre l) = show l

-----------------------------------------------------------------

class Collage sup inf where
  dec ::  sup  -> [inf]
  inc :: [inf] -> sup

instance Collage Texte Paragraphe where
  dec (Texte t) = map Paragraphe $ DT.splitOn "\n" t
  inc           = Texte . DT.intercalate "\n" . map (\(Paragraphe t) -> t)

instance Collage Paragraphe Phrase where
  dec (Paragraphe t) = map Phrase $ sentences t
  inc                = Paragraphe . DT.unwords . map (\(Phrase p) -> p)

instance Collage Phrase MultiTerme where
  dec (Phrase t) = map MultiTerme $ DT.words t
  inc            = Phrase . DT.unwords . map (\(MultiTerme p) -> p)

instance Collage MultiTerme Mot where
  dec (MultiTerme mt) = map Mot $ DT.words mt
  inc                 = MultiTerme . DT.intercalate " " . map (\(Mot m) -> m)

-------------------------------------------------------------------
-- Contexts of text
sentences :: Text -> [Text]
sentences txt = map DT.pack $ segment $ DT.unpack txt

sentences' :: Text -> [Text]
sentences' txt = split isCharStop txt

isCharStop :: Char -> Bool
isCharStop c = c `elem` ['.','?','!']

unsentences :: [Text] -> Text
unsentences txts = DT.intercalate " " txts

-- | https://en.wikipedia.org/wiki/Text_mining
testText_en :: Text
testText_en = DT.pack "Text mining, also referred to as text data mining, roughly equivalent to text analytics, is the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning. Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interestingness. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities). Text analysis involves information retrieval, lexical analysis to study word frequency distributions, pattern recognition, tagging/annotation, information extraction, data mining techniques including link and association analysis, visualization, and predictive analytics. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods. A typical application is to scan a set of documents written in a natural language and either model the document set for predictive classification purposes or populate a database or search index with the information extracted."


testText_en_2 :: Text
testText_en_2 = DT.pack "It is hard to detect important articles in a specific context. Information retrieval techniques based on full text search can be inaccurate to identify main topics and they are not able to provide an indication about the importance of the article. Generating a citation network is a good way to find most popular articles but this approach is not context aware. The text around a citation mark is generally a good summary of the referred article. So citation context analysis presents an opportunity to use the wisdom of crowd for detecting important articles in a context sensitive way. In this work, we analyze citation contexts to rank articles properly for a given topic. The model proposed uses citation contexts in order to create a directed and edge-labeled citation network based on the target topic. Then we apply common ranking algorithms in order to find important articles in this newly created network. We showed that this method successfully detects a good subset of most prominent articles in a given topic. The biggest contribution of this approach is that we are able to identify important articles for a given search term even though these articles do not contain this search term. This technique can be used in other linked documents including web pages, legal documents, and patents as well as scientific papers."


-- | https://fr.wikipedia.org/wiki/Fouille_de_textes
testText_fr :: Text
testText_fr = DT.pack "La fouille de textes ou « l'extraction de connaissances » dans les textes est une spécialisation de la fouille de données et fait partie du domaine de l'intelligence artificielle. Cette technique est souvent désignée sous l'anglicisme text mining. Elle désigne un ensemble de traitements informatiques consistant à extraire des connaissances selon un critère de nouveauté ou de similarité dans des textes produits par des humains pour des humains. Dans la pratique, cela revient à mettre en algorithme un modèle simplifié des théories linguistiques dans des systèmes informatiques d'apprentissage et de statistiques. Les disciplines impliquées sont donc la linguistique calculatoire, l'ingénierie des langues, l'apprentissage artificiel, les statistiques et l'informatique."

termTests :: Text
termTests = "It is hard to detect important articles in a specific context. Information retrieval techniques based on full text search can be inaccurate to identify main topics and they are not able to provide an indication about the importance of the article. Generating a citation network is a good way to find most popular articles but this approach is not context aware. The text around a citation mark is generally a good summary of the referred article. So citation context analysis presents an opportunity to use the wisdom of crowd for detecting important articles in a context sensitive way. In this work, we analyze citation contexts to rank articles properly for a given topic. The model proposed uses citation contexts in order to create a directed and edge-labeled citation network based on the target topic. Then we apply common ranking algorithms in order to find important articles in this newly created network. We showed that this method successfully detects a good subset of most prominent articles in a given topic. The biggest contribution of this approach is that we are able to identify important articles for a given search term even though these articles do not contain this search term. This technique can be used in other linked documents including web pages, legal documents, and patents as well as scientific papers."


-- | Ngrams Test
-- >> ngramsTest testText
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

