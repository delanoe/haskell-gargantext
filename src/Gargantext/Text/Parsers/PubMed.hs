{-|
Module      : Gargantext.Text.Parsers.PubMed
Description : Parser for Wikimedia dump
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This version of Parsers fixes the Date of publication in Gargantext
(V3) parser of PubMed. Indeed, we can not rely neither on Journal
Publication Date neither on Article publication date, which are
incomplete structurally but for its interpretation too. Then, to
simplify and uniformize data, date of publication of database insertion
is used.

TODO:
- Add authors
- factorize
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Parsers.PubMed where


import Data.Conduit.List as CL hiding (catMaybes, head)
import Control.Monad (join)
import GHC.IO (FilePath)
import Prelude (read)
import Gargantext.Prelude
import Control.Monad.Catch (MonadThrow)
import Data.Maybe (Maybe)
import Data.Monoid (mconcat)
import Text.XML.Stream.Parse
import Data.Conduit (runConduit, (.|), ConduitT)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Data.Time.Segment (jour)
import Data.Time (UTCTime(..))
import qualified Data.ByteString.Lazy as DBL
import Gargantext.Text.Parsers.Wikimedia


data PubMed = 
     PubMed { pubmed_article :: PubMedArticle
            , pubmed_date    :: PubMedData
     } deriving Show

data PubMedArticle =
     PubMedArticle { pubmed_title   :: Maybe Text
                   , pubmed_journal :: Maybe Text
                   , pubmed_abstract :: Maybe [Text]
                   }
                   deriving (Show)

data PubMedData =
     PubMedData { pubmedData_date     :: UTCTime
                , pubmedData_year     :: Integer
                , pubmedData_month    :: Int
                , pubmedData_day      :: Int
                } deriving (Show)

readPubMedFile :: FilePath -> IO [PubMed]
readPubMedFile fp = do
  input <- DBL.readFile fp
  pubMedParser input

pubMedParser :: DBL.ByteString -> IO [PubMed]
pubMedParser bstring = runConduit $ parseLBS def bstring
                                 .| parseArticleSet
                                 .| CL.consume

parseArticleSet :: MonadThrow m => ConduitT Event PubMed m ()
parseArticleSet = do
  as <- force "force" $ tagIgnoreAttrs "PubmedArticleSet" $ manyYield parsePubMedArticle
  return as

parsePubMedArticle :: MonadThrow m => ConduitT Event o m (Maybe PubMed)
parsePubMedArticle = do
  articles <- tagIgnoreAttrs "PubmedArticle" parsePubMedArticle'
  return articles

parsePubMedArticle' :: MonadThrow m => ConduitT Event o m (PubMed)
parsePubMedArticle' = do
  article <- force "MedlineCitation" $ tagIgnoreAttrs "MedlineCitation" parseMedlineCitation
  dates <- tagIgnoreAttrs "PubmedData" $ do
    dates' <- tagIgnoreAttrs "History" $ many $ tagIgnoreAttrs "PubMedPubDate" $ do
      y' <- force "Year"  $ tagIgnoreAttrs "Year"  content
      m' <- force "Month" $ tagIgnoreAttrs "Month" content
      d' <- force "Day"   $ tagIgnoreAttrs "Day"   content
      _ <- many $ ignoreAnyTreeContent
      return (read $ unpack y', read $ unpack m', read $ unpack d')
    _ <- many $ ignoreAnyTreeContent
    return dates'
  _ <- many $ ignoreAnyTreeContent
  let (y,m,d) = maybe (1,1,1) identity $ join $ fmap head $ reverse <$> join dates
  return $ PubMed (article) (PubMedData (jour y m d) y m d)

parseMedlineCitation :: MonadThrow m => ConduitT Event o m PubMedArticle
parseMedlineCitation = do
  a <- force "article" $ manyTagsUntil "Article" parseArticle
  _ <- many $ ignoreAnyTreeContent
  return a

parseArticle :: MonadThrow m => ConduitT Event o m PubMedArticle
parseArticle = do
  journal  <- force "journal" $ manyTagsUntil "Journal" $ do
    j <- manyTagsUntil "Title" content
    _ <- many $ ignoreAnyTreeContent
    return j
  
  title    <- do
    t <- manyTagsUntil "ArticleTitle" content
    return t
  
  abstracts <- do
    as <- manyTagsUntil "Abstract" $ many $ do
      txt <- tagIgnoreAttrs "AbstractText" $ do
        c <- content
        _ <- many $ ignoreAnyTreeContent
        return c
      _ <- many $ ignoreAnyTreeContent
      return txt
    return as
  -- TODO add authos

  _ <- many $ ignoreAnyTreeContent
  return $ PubMedArticle title journal abstracts


pubMedData :: DBL.ByteString
pubMedData = mconcat
  [ "<?xml version=\"1.0\"?>\n"
  , "<!DOCTYPE PubmedArticleSet PUBLIC \"-//NLM//DTD PubMedArticle, 1st June 2018//EN\" \"https://dtd.nlm.nih.gov/ncbi/pubmed/out/pubmed_180601.dtd\">\n"
  , "<PubmedArticleSet>\n"
  , "<PubmedArticle>\n"
  , " <MedlineCitation Status=\"Publisher\" Owner=\"NLM\">\n"
  , "        <PMID Version=\"1\">30357468</PMID>\n"
  , "        <DateRevised>\n"
  , "           <Year>2018</Year>\n"
  , "        </DateRevised>\n"
  , "        <Article PubModel=\"Print-Electronic\">\n"
  , "          <Journal>\n"
  , "            <ISSN IssnType=\"Electronic\">1432-1076</ISSN>\n"
  , "            <Title>European journal of pediatrics</Title>\n"
  , "          </Journal>\n"
  , "          <ArticleTitle>Title of the Article</ArticleTitle>\n"
  , "          <ELocationID EIdType=\"doi\" ValidYN=\"Y\">10.1007/s00431-018-3270-3</ELocationID>\n"
  , "          <Abstract>\n"
  , "              <AbstractText>Abstract Text.</AbstractText>\n"
  , "          </Abstract>\n"
  , "          <AuthorList>\n"
  , "          </AuthorList>\n"
  , "        </Article>\n"
  , " </MedlineCitation>\n"
  , " <PubmedData>\n"
  , "   <History>\n"
  , "   </History>\n"
  , " </PubmedData>\n"
  , "</PubmedArticle>\n"
  , "</PubmedArticleSet>\n"
  ]



