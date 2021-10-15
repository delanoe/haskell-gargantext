{-|
Module      : Gargantext.Core.Text.Corpus.Parsers
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

{-# LANGUAGE PackageImports    #-}

module Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), clean, parseFile, cleanText, parseFormat)
    where

import "zip" Codec.Archive.Zip (withArchive, getEntry, getEntries)
import Control.Concurrent.Async as CCA (mapConcurrently)
import Data.Attoparsec.ByteString (parseOnly, Parser)
import Data.Either(Either(..))
import Data.Either.Extra (partitionEithers)
import Data.List (concat, lookup)
import Data.Ord()
import Data.String (String())
import Data.String()
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra (both, first, second)
import System.FilePath (FilePath(), takeExtension)
import qualified Data.ByteString       as DB
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy  as DBL
import qualified Data.Map              as DM
import qualified Data.Text             as DT
import qualified Prelude as Prelude
import System.IO.Temp (emptySystemTempFile)

import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import Gargantext.Core.Text.Corpus.Parsers.CSV (parseHal, parseHal', parseCsv, parseCsv')
import Gargantext.Core.Text.Corpus.Parsers.RIS.Presse (presseEnrich)
-- import Gargantext.Core.Text.Learn (detectLangDefault)
import qualified Gargantext.Core.Text.Corpus.Parsers.Date as Date
import qualified Gargantext.Core.Text.Corpus.Parsers.RIS  as RIS
import qualified Gargantext.Core.Text.Corpus.Parsers.WOS  as WOS
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
data FileFormat = WOS | RIS | RisPresse
                | CsvGargV3 | CsvHal
                | ZIP
  deriving (Show)

-- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODT        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :


parseFormat :: FileFormat -> DB.ByteString -> IO (Either Prelude.String [HyperdataDocument])
parseFormat CsvGargV3 bs = pure $ parseCsv' $ DBL.fromStrict bs
parseFormat CsvHal    bs = pure $ parseHal' $ DBL.fromStrict bs
parseFormat RisPresse bs = do
  docs <- mapM (toDoc RIS)
          <$> snd
          <$> enrichWith RisPresse
          $ partitionEithers
          $ [runParser'  RisPresse bs]
  pure $ Right docs
parseFormat WOS bs = do
  docs <- mapM (toDoc WOS)
          <$> snd
          <$> enrichWith WOS
          $ partitionEithers
          $ [runParser'  WOS bs]
  pure $ Right docs
parseFormat ZIP bs = do
  path <- emptySystemTempFile "parsed-zip"
  DB.writeFile path bs
  parsedZip <- withArchive path $ do
    DM.keys <$> getEntries
  pure $ Left $ "Not implemented for ZIP, parsedZip" <> show parsedZip
parseFormat _ _ = undefined

-- | Parse file into documents
-- TODO manage errors here
-- TODO: to debug maybe add the filepath in error message
parseFile :: FileFormat -> FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseFile CsvHal    p = parseHal p
parseFile CsvGargV3 p = parseCsv p
parseFile RisPresse p = do
  docs <- join $ mapM (toDoc RIS) <$> snd <$> enrichWith RisPresse <$> readFileWith RIS p
  pure $ Right docs
parseFile WOS       p = do
  docs <- join $ mapM (toDoc WOS) <$> snd <$> enrichWith WOS       <$> readFileWith WOS p
  pure $ Right docs
parseFile ff        p = do
  docs <- join $ mapM (toDoc ff)  <$> snd <$> enrichWith ff        <$> readFileWith ff  p
  pure $ Right docs

toDoc :: FileFormat -> [(Text, Text)] -> IO HyperdataDocument
-- TODO use language for RIS
toDoc ff d = do
      -- let abstract = lookup "abstract" d
      let lang = EN -- maybe EN identity (join $ detectLangDefault <$> (fmap (DT.take 50) abstract))

      let dateToParse = DT.replace "-" " " <$> lookup "PY" d <> Just " " <> lookup "publication_date" d

      (utcTime, (pub_year, pub_month, pub_day)) <- Date.dateSplit lang  dateToParse

      pure $ HyperdataDocument { _hd_bdd = Just $ DT.pack $ show ff
                               , _hd_doi = lookup "doi" d
                               , _hd_url = lookup "URL" d
                               , _hd_uniqId = Nothing
                               , _hd_uniqIdBdd = Nothing
                               , _hd_page = Nothing
                               , _hd_title = lookup "title" d
                               , _hd_authors = Nothing
                               , _hd_institutes = lookup "authors" d
                               , _hd_source = lookup "source" d
                               , _hd_abstract = lookup "abstract" d
                               , _hd_publication_date = fmap (DT.pack . show) utcTime
                               , _hd_publication_year = pub_year
                               , _hd_publication_month = pub_month
                               , _hd_publication_day = pub_day
                               , _hd_publication_hour = Nothing
                               , _hd_publication_minute = Nothing
                               , _hd_publication_second = Nothing
                               , _hd_language_iso2 = Just $ (DT.pack . show) lang }

enrichWith :: FileFormat
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith RisPresse = enrichWith' presseEnrich
enrichWith WOS       = enrichWith' (map (first WOS.keys))
enrichWith _         = enrichWith' identity


enrichWith' ::      ([(DB.ByteString, DB.ByteString)] -> [(DB.ByteString, DB.ByteString)])
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith' f = second (map both' . map f . concat)
  where
    both'   = map (both decodeUtf8)



readFileWith :: FileFormat -> FilePath
       -> IO ([ParseError], [[[(DB.ByteString, DB.ByteString)]]])
readFileWith format path = do
    files <- case takeExtension path of
              ".zip" -> openZip              path
              _      -> pure <$> clean <$> DB.readFile path
    partitionEithers <$> mapConcurrently (runParser format) files


-- | withParser:
-- According to the format of the text, choose the right parser.
-- TODO  withParser :: FileFormat -> Parser [Document]
withParser :: FileFormat -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS = WOS.parser
withParser RIS = RIS.parser
--withParser ODT = odtParser
--withParser XML = xmlParser
withParser _   = panic "[ERROR] Parser not implemented yet"

runParser :: FileFormat -> DB.ByteString
          -> IO (Either String [[(DB.ByteString, DB.ByteString)]])
runParser format text = pure $ runParser' format text

runParser' :: FileFormat -> DB.ByteString
          -> (Either String [[(DB.ByteString, DB.ByteString)]])
runParser' format text = parseOnly (withParser format) text

openZip :: FilePath -> IO [DB.ByteString]
openZip fp = do
    entries <- withArchive fp (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive fp (getEntry s)) entries
    pure bs

cleanText :: Text -> Text
cleanText = cs . clean . cs

clean :: DB.ByteString -> DB.ByteString
clean txt = DBC.map clean' txt
  where
    clean' '’' = '\''
    clean' '\r' = ' '
    clean' '\t' = ' '
    clean' ';' = '.'
    clean' c  = c
