{-|
Module      : Gargantext.Text.Parsers
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Gargantext enables analyzing semi-structured text that should be parsed
in order to be analyzed.

The parsers suppose we know the format of the Text (TextFormat data
type) according to which the right parser is chosen among the list of
available parsers.

This module mainly describe how to add a new parser to Gargantext,
please follow the types.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Parsers (parse, FileFormat(..), clean, parseDocs)
    where

import System.FilePath (FilePath(), takeExtension)
import "zip" Codec.Archive.Zip (withArchive, getEntry, getEntries)

import Control.Monad (join)
import qualified Data.Time as DT
import Data.Either.Extra (partitionEithers)
import Data.Time (UTCTime(..))
import Data.List (concat)
import qualified Data.Map        as DM
import qualified Data.ByteString as DB
import Data.Ord()
import Data.String()
import Data.Either(Either(..))
import Data.Attoparsec.ByteString (parseOnly, Parser)

import Data.Text (Text)
import qualified Data.Text as DT

-- Activate Async for to parse in parallel
import Control.Concurrent.Async as CCA (mapConcurrently)

import Data.Text.Encoding (decodeUtf8)
import Data.String (String())
import Data.List (lookup)

------------------------------------------------------------------------
import Gargantext.Core (Lang(..))
import Gargantext.Prelude
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Gargantext.Text.Parsers.WOS (wosParser)
import Gargantext.Text.Parsers.RIS (risParser)
import Gargantext.Text.Parsers.Date (parseDate)
import Gargantext.Text.Parsers.CSV (parseHal)
import Gargantext.Text.Terms.Stop (detectLang)
------------------------------------------------------------------------

type ParseError = String
--type Field      = Text
--type Document   = DM.Map Field Text
--type FilesParsed = DM.Map FilePath FileParsed
--data FileParsed  = FileParsed { _fileParsed_errors ::  Maybe ParseError
--                              , _fileParsed_result :: [Document]
--                              } deriving (Show)


-- | According to the format of Input file,
-- different parser are available.
data FileFormat = WOS | RIS | CsvHalFormat -- | CsvGargV3
  deriving (Show)

-- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODT        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :
--                             -- > http://chrisdone.com/posts/fast-haskell-c-parsing-xml

-- TODO: to debug maybe add the filepath in error message


-- | Parse file into documents
-- TODO manage errors here
parseDocs :: FileFormat -> FilePath -> IO [HyperdataDocument]
parseDocs ff    path = join $ mapM (toDoc ff) <$> snd <$> parse ff path
parseDocs CsvHalFormat p = parseHal p

type Year  = Int
type Month = Int
type Day   = Int

-- | Parse date to Ints
-- TODO add hours, minutes and seconds
parseDate' :: Lang -> Maybe Text -> IO (Maybe UTCTime, (Maybe Year, Maybe Month, Maybe Day))
parseDate' _ Nothing    = pure (Nothing, (Nothing, Nothing, Nothing))
parseDate' l (Just txt) = do
  utcTime <- parseDate l txt
  let (UTCTime day _) = utcTime
  let (y,m,d) = DT.toGregorian day
  pure (Just utcTime, (Just (fromIntegral y), Just m,Just d))


toDoc :: FileFormat -> [(Text, Text)] -> IO HyperdataDocument
-- TODO use language for RIS
toDoc ff d = do
      let abstract = lookup "abstract" d
      let lang = maybe EN identity (join $ detectLang <$> (fmap (DT.take 50) abstract))
      
      let dateToParse = DT.replace "-" " " <$> lookup "PY" d <> Just " " <> lookup "publication_date" d
      
      (utcTime, (pub_year, pub_month, pub_day)) <- parseDate' lang  dateToParse

      pure $ HyperdataDocument (Just $ DT.pack $ show ff)
                               (lookup "doi" d)
                               (lookup "URL" d)
                                Nothing
                                Nothing
                                Nothing
                               (lookup "title" d)
                                Nothing
                               (lookup "authors" d)
                               (lookup "source" d)
                               (lookup "abstract" d)
                               (fmap (DT.pack . show) utcTime)
                               (pub_year)
                               (pub_month)
                               (pub_day)
                               Nothing
                               Nothing
                               Nothing
                               (Just $ (DT.pack . show) lang)
toDoc _ _ = undefined

parse :: FileFormat -> FilePath -> IO ([ParseError], [[(Text, Text)]])
parse format path = do
    files <- case takeExtension path of
              ".zip" -> openZip              path
              _      -> pure <$> DB.readFile path
    (as, bs) <- partitionEithers <$> mapConcurrently (runParser format) files
    pure (as, map toText $ concat bs)
      where
        -- TODO : decode with bayesian inference on encodings
        toText = map (\(a,b) -> (decodeUtf8 a, decodeUtf8 b))


-- | withParser:
-- According to the format of the text, choose the right parser.
-- TODO  withParser :: FileFormat -> Parser [Document]
withParser :: FileFormat -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS = wosParser
withParser RIS = risParser
--withParser ODT = odtParser
--withParser XML = xmlParser
withParser _   = panic "[ERROR] Parser not implemented yet"

runParser :: FileFormat -> DB.ByteString
          -> IO (Either String [[(DB.ByteString, DB.ByteString)]])
runParser format text = pure $ parseOnly (withParser format) text

openZip :: FilePath -> IO [DB.ByteString]
openZip fp = do
    entries <- withArchive fp (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive fp (getEntry s)) entries
    pure bs

clean :: Text -> Text
clean txt = DT.map clean' txt
  where
    clean' '’' = '\''
    clean' c  = c

