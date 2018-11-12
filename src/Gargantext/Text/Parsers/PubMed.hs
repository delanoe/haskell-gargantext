{-|
Module      : Gargantext.Text.Parsers.PubMed
Description : Parser for Wikimedia dump
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

@Gargantext.Text.Parsers.Wikimedia@:
This module provide a parser for wikipedia dump.
This include an xml parser for wikipedia's xml
and an wikimedia to plaintext converter for the wikipedia text field
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Parsers.PubMed where

{-
import Data.Conduit
import Data.XML.Types (Event, Name)
import Text.Pandoc
import Data.Text as T
import Data.Either
-}

import Control.Monad (join)
import GHC.IO (FilePath)
import Prelude (read)
import Gargantext.Prelude
import Control.Applicative ((<*))
import Control.Monad.Catch (MonadThrow)
import Data.Maybe
import Data.Monoid (mconcat)
import Text.XML.Stream.Parse
import Data.Conduit (runConduit, (.|), ConduitT)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as DBL
import Gargantext.Text.Parsers.Wikimedia


issueXml :: Maybe [PubMedArticle]
issueXml = pubMedParser pubMedData

data PubMedArticle =
     PubMedArticle { pubmed_title   :: Maybe Text
                   , pubmed_journal :: Maybe Text
                   }
     deriving (Show)

readPubMedFile :: FilePath -> IO (Maybe [PubMedArticle])
readPubMedFile fp = do
  input <- DBL.readFile fp
  pure $ pubMedParser input


pubMedParser :: DBL.ByteString -> Maybe [PubMedArticle]
pubMedParser bstring = runConduit $ parseLBS def bstring .| force "Pubmed" parseArticles

parseArticles :: MonadThrow m => ConduitT Event o m (Maybe [PubMedArticle])
parseArticles = tagIgnoreAttrs "PubmedArticleSet" $ many parseArticle

parseArticle :: MonadThrow m => ConduitT Event o m (Maybe PubMedArticle)
parseArticle = tagIgnoreAttrs "PubmedArticle" parseMedlineCitation

parseMedlineCitation :: MonadThrow m => ConduitT Event o m PubMedArticle
parseMedlineCitation = force "medlineCitation" $ tagIgnoreAttrs "MedlineCitation" $ do
  _ <- manyTagsUntil_ "Article"
  journal <- tagIgnoreAttrs "Journal"     $ force "journal" $ manyTagsUntil "Title"     content
  title   <- manyTagsUntil "ArticleTitle" $ force "title"   $ manyTagsUntil "ArticleTitle" content
  _ <- many $ ignoreAnyTreeContent
  return $ PubMedArticle title journal


pubMedData :: DBL.ByteString
pubMedData = mconcat
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE PubmedArticleSet PUBLIC \"-//NLM//DTD PubMedArticle, 1st June 2018//EN\" \"https://dtd.nlm.nih.gov/ncbi/pubmed/out/pubmed_180601.dtd\">"
  , "<PubmedArticleSet>"
  , "<PubmedArticle>"
  , "<MedlineCitation Status=\"Publisher\" Owner=\"NLM\">"
  , "        <PMID Version=\"1\">30357468</PMID>"
  , "        <DateRevised>"
  , "           <Year>2018</Year>"
  , "        </DateRevised>"
  , "        <Article PubModel=\"Print-Electronic\">"
  , "          <Journal>"
  , "            <ISSN IssnType=\"Electronic\">1432-1076</ISSN>"
  , "            <Title>European journal of pediatrics</Title>"
  , "          </Journal>"
  , "          <ArticleTitle>European journal of pediatrics</ArticleTitle>"
  , "        </Article>"
  , "</MedlineCitation>"
  , "</PubmedArticle>"
  , "</PubmedArticleSet>"
  ]


