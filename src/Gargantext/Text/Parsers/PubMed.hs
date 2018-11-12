{-|
Module      : Gargantext.Text.Parsers.PubMed
Description : Parser for Wikimedia dump
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Parsers.PubMed where



import Control.Monad (void)
import Data.Conduit.List as CL hiding (catMaybes)
import Control.Monad (join)
import GHC.IO (FilePath)
import Prelude (read, print)
import Gargantext.Prelude
import Control.Applicative ((<*))
import Control.Monad.Catch (MonadThrow)
import Data.Maybe (Maybe, catMaybes)
import Data.Monoid (mconcat)
import Text.XML.Stream.Parse
import Data.Conduit (runConduit, (.|), ConduitT)
import Data.Text (Text, unpack, concat)
import Data.XML.Types (Event)
import Data.ByteString (ByteString)
import Data.Time.Segment (jour)
import Data.Time (UTCTime(..))
import qualified Data.ByteString.Lazy as DBL
import Gargantext.Text.Parsers.Wikimedia


data PubMedArticle =
     PubMedArticle { pubmed_title   :: Maybe Text
                   , pubmed_journal :: Maybe Text
                   , pubmed_abstract :: Maybe [Text]
                   , pubmed_date     :: UTCTime
                   , pubmed_year     :: Integer
                   , pubmed_month    :: Int
                   , pubmed_day      :: Int
                   }
     deriving (Show)

readPubMedFile :: FilePath -> IO ()
readPubMedFile fp = do
  input <- DBL.readFile fp
  pubMedParser input


pubMedParser :: DBL.ByteString -> IO ()
pubMedParser bstring = runConduit $ parseLBS def bstring
                                 .| parseArticleSet
                                 .| CL.mapM_ print

--parseArticleSet :: MonadThrow m => ConduitT Event o m [PubMedArticle]
parseArticleSet = do
  as <- force "force" $ tagIgnoreAttrs "PubmedArticleSet" $ manyYield parsePubMedArticle
  -- _ <- many $ ignoreAnyTreeContent
  return as

parsePubMedArticle :: MonadThrow m => ConduitT Event o m (Maybe PubMedArticle)
parsePubMedArticle = do
  articles <- force "PubmedArticle" $ tagIgnoreAttrs "PubmedArticle" parsePubMedArticle'
  --_ <- many $ ignoreAnyTreeContent
  return articles

parsePubMedArticle' :: MonadThrow m => ConduitT Event o m (Maybe PubMedArticle)
parsePubMedArticle' = do
  pubmed_article <- tagIgnoreAttrs "MedlineCitation" parseMedlineCitation
  --_ <- tagIgnoreAttrs "PubmedData" content
  _ <- many $ ignoreAnyTreeContent
  return pubmed_article

parseMedlineCitation :: MonadThrow m => ConduitT Event o m PubMedArticle
parseMedlineCitation = do
  a <- force "article" $ manyTagsUntil "Article" parseArticle
  _ <- many $ ignoreAnyTreeContent
  return a

parseArticle :: MonadThrow m => ConduitT Event o m PubMedArticle
parseArticle = do
  (journal,maybePubDate)  <- force "journal" $ manyTagsUntil "Journal" $ do
    maybePubDate' <- manyTagsUntil "JournalIssue" $ do
      maybePubDate'' <- manyTagsUntil "PubDate" $ do
        y <- tagIgnoreAttrs "Year"  content
        m <- tagIgnoreAttrs "Month" content
        d <- tagIgnoreAttrs "Day"   content
        return (y, m, d)
      return maybePubDate''

    j <- manyTagsUntil "Title" content
    _ <- many $ ignoreAnyTreeContent
    return (j,join maybePubDate')
  
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

  (year, month, day) <- case maybePubDate of
    Nothing -> force "ArticleDate" $ manyTagsUntil "ArticleDate" $ do
      y <- force "Year"  $ tagIgnoreAttrs "Year"  content
      m <- force "Month" $ tagIgnoreAttrs "Month" content
      d <- force "Day"   $ tagIgnoreAttrs "Day"   content
      return (read $ unpack y, read $ unpack m, read $ unpack d)
    Just (Just y, Just m, Just d)  -> return (read $ unpack "1", read $ unpack "3", read $ unpack "3")
    _ -> panic "error date"

  _ <- many $ ignoreAnyTreeContent
  return $ PubMedArticle title journal abstracts (jour year month day) year month day


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



