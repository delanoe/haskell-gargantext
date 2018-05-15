{-|
Module      : Gargantext.Text.Parsers.CSV
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gargantext.Text.Parsers.CSV where

import GHC.Real (round)
import GHC.IO (FilePath)
import Control.Applicative

import Data.Char (ord)
import Data.Csv
import Data.Either (Either(Left, Right))
import Data.List (concat)
import Data.String (IsString)
import Data.Text (Text, pack, unpack, length)
import qualified Data.ByteString.Lazy as BL

import Data.Vector (Vector)
import qualified Data.Vector as V
import Safe (tailMay)
import Text.HTML.TagSoup

import Gargantext.Text
import Gargantext.Prelude hiding (length)

---------------------------------------------------------------
data Doc = Doc
    { d_docId  :: !Int
    , d_title  :: !Text
    , d_source :: !Text
    , d_publication_year  :: !Int
    , d_publication_month :: !Int
    , d_publication_day   :: !Int
    , d_abstract          :: !Text
    , d_authors           :: !Text
    }
    deriving (Show)
---------------------------------------------------------------
toDocs :: Vector CsvDoc -> [Doc]
toDocs v = V.toList 
         $ V.zipWith (\nId (CsvDoc t s py pm pd abst auth)
                       -> Doc nId t s py pm pd abst auth )
                       (V.enumFromN 1 (V.length v')) v''
      where
        m  = docsSize v
        v' = V.concatMap (splitDoc m Paragraph) v

        m' = docsSize v
        v'' = V.concatMap (splitDoc m' Sentences) v'
        
        m'' = docsSize v'
        v''' = V.concatMap (splitDoc m' Sentences) v''

---------------------------------------------------------------
fromDocs :: Vector Doc -> Vector CsvDoc
fromDocs docs = V.map fromDocs' docs
  where
    fromDocs' (Doc _ t s py pm pd abst auth) = (CsvDoc t s py pm pd abst auth)

---------------------------------------------------------------
-- | Split a document in its context
-- TODO adapt the size of the paragraph according to the corpus average

data SplitBy = Paragraph | Sentences | Chars

splitDoc :: Mean -> SplitBy -> CsvDoc -> Vector CsvDoc
splitDoc m splt doc = let docSize = (length $ c_abstract doc) in
                          if docSize > 1000
                            then
                              if (mod (round m) docSize) >= 10
                                then
                                  splitDoc' splt doc
                                else
                                  V.fromList [doc]
                            else
                              V.fromList [doc]


splitDoc' :: SplitBy -> CsvDoc -> Vector CsvDoc
splitDoc' splt (CsvDoc t s py pm pd abst auth) = V.fromList $ [firstDoc] <> nextDocs
    where
      firstDoc = CsvDoc t s py pm pd firstAbstract auth
      firstAbstract = head' abstracts
      
      nextDocs = map (\txt -> CsvDoc (head' $ sentences txt) s py pm pd (unsentences $ tail' $ sentences txt) auth) (tail' abstracts)
      
      abstracts    = (splitBy splt) abst
      head' x = maybe ""   identity (head x)
      tail' x = maybe [""] identity (tailMay x)


splitBy :: SplitBy -> Text -> [Text]
splitBy Chars     = map pack . chunkAlong 1000 1 . unpack
splitBy Sentences = map unsentences . chunkAlong 20 1  . sentences
splitBy Paragraph = map removeTag   . filter isTagText . parseTags
  where
    removeTag :: IsString p => Tag p -> p
    removeTag (TagText x) = x
    removeTag (TagComment x) = x
    removeTag _          = ""

---------------------------------------------------------------
---------------------------------------------------------------
type Mean = Double

docsSize :: Vector CsvDoc -> Mean
docsSize csvDoc = mean ls
  where
    ls = V.toList $ V.map (fromIntegral . length . c_abstract) csvDoc


---------------------------------------------------------------
data CsvDoc = CsvDoc
    { c_title  :: !Text
    , c_source :: !Text
    , c_publication_year  :: !Int
    , c_publication_month :: !Int
    , c_publication_day   :: !Int
    , c_abstract          :: !Text
    , c_authors           :: !Text
    }
    deriving (Show)

instance FromNamedRecord CsvDoc where
  parseNamedRecord r = CsvDoc <$> r .: "title"
                              <*> r .: "source"
                              <*> r .: "publication_year"
                              <*> r .: "publication_month"
                              <*> r .: "publication_day"
                              <*> r .: "abstract"
                              <*> r .: "authors"

instance ToNamedRecord CsvDoc where
  toNamedRecord (CsvDoc t s py pm pd abst aut) = 
    namedRecord [ "title"  .= t
                , "source" .= s
                , "publication_year"  .= py
                , "publication_month" .= pm
                , "publication_day"   .= pd
                , "abstract"          .= abst
                , "authors"           .= aut
                ]


csvDecodeOptions :: DecodeOptions
csvDecodeOptions = (defaultDecodeOptions
                      {decDelimiter = fromIntegral $ ord '\t'}
                    )

csvEncodeOptions :: EncodeOptions
csvEncodeOptions = ( defaultEncodeOptions 
                      {encDelimiter = fromIntegral $ ord '\t'}
                    )


readCsv :: FilePath -> IO (Header, Vector CsvDoc)
readCsv fp = do
    csvData <- BL.readFile fp
    case decodeByNameWith csvDecodeOptions csvData of
      Left e    -> panic (pack e)
      Right csvDocs -> pure csvDocs


writeCsv :: FilePath -> (Header, Vector CsvDoc) -> IO ()
writeCsv fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith csvEncodeOptions h (V.toList vs)

