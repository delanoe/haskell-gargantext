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

module Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), FileType(..), clean, parseFile, cleanText, parseFormatC, splitOn)
    where

-- import Gargantext.Core.Text.Learn (detectLangDefault)
import "zip" Codec.Archive.Zip (withArchive, getEntry, getEntries)
import Conduit
import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Monad (join)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Attoparsec.ByteString (parseOnly, Parser)
import Data.Either(Either(..))
import Data.Either.Extra (partitionEithers)
import Data.List (concat, lookup)
import Data.Ord()
import Data.String (String())
import Data.String()
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra (both, first, second)
import Gargantext.API.Node.Corpus.New.Types (FileFormat(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.CSV (parseHal, parseCsv, parseCsvC)
import Gargantext.Core.Text.Corpus.Parsers.RIS.Presse (presseEnrich)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import System.FilePath (FilePath(), takeExtension)
import System.IO.Temp (emptySystemTempFile)
import qualified Data.ByteString       as DB
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy  as DBL
import qualified Data.Map              as DM
import qualified Data.Text             as DT
import qualified Gargantext.Core.Text.Corpus.Parsers.Date as Date
import qualified Gargantext.Core.Text.Corpus.Parsers.RIS  as RIS
import qualified Gargantext.Core.Text.Corpus.Parsers.WOS  as WOS
import qualified Prelude
import Gargantext.Database.Query.Table.Ngrams (NgramsType(..))
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
data FileType = WOS | RIS | RisPresse | CsvGargV3 | CsvHal
  deriving (Show)

-- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODT        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :

parseFormatC :: MonadBaseControl IO m
             => FileType
             -> FileFormat
             -> DB.ByteString
             -> m (Either Prelude.String (Maybe Integer, ConduitT () HyperdataDocument IO ()))
parseFormatC CsvGargV3 Plain bs = do
  let eParsedC = parseCsvC $ DBL.fromStrict bs
  case eParsedC of
    Left err -> pure $ Left err
    Right (mLen, parsedC) -> pure $ Right (mLen, transPipe (pure . runIdentity) parsedC)
parseFormatC CsvHal    Plain bs = do
  let eParsedC = parseCsvC $ DBL.fromStrict bs
  case eParsedC of
    Left err -> pure $ Left err
    Right (mLen, parsedC) -> pure $ Right (mLen, transPipe (pure . runIdentity) parsedC)
parseFormatC RisPresse Plain bs = do
  --docs <- enrichWith RisPresse
  let eDocs = runParser' RisPresse bs
  pure $ (\docs ->
            ( Just $ fromIntegral $ length docs
            , yieldMany docs
              .| mapC presseEnrich
              .| mapC (map $ both decodeUtf8)
              .| mapMC (toDoc RIS)) ) <$> eDocs
parseFormatC WOS Plain bs = do
  let eDocs = runParser' WOS bs
  pure $ (\docs ->
            ( Just $ fromIntegral $ length docs
            , yieldMany docs
              .| mapC (map $ first WOS.keys)
              .| mapC (map $ both decodeUtf8)
              .| mapMC (toDoc WOS)) ) <$> eDocs
parseFormatC ft ZIP bs = do
  path <- liftBase $ emptySystemTempFile "parsed-zip"
  liftBase $ DB.writeFile path bs
  fileContents <- liftBase $ withArchive path $ do
    files <- DM.keys <$> getEntries
    mapM getEntry files
  --printDebug "[parseFormatC] fileContents" fileContents
  eContents <- mapM (parseFormatC ft Plain) fileContents
  --printDebug "[parseFormatC] contents" contents
  --pure $ Left $ "Not implemented for ZIP"
  let (errs, contents) = partitionEithers eContents
  case errs of
    [] ->
      case contents of
        [] -> pure $ Left "No files in zip"
        _  -> do
          let lenghts = fst <$> contents
          let contents' = snd <$> contents
          let totalLength = sum $ sum <$> lenghts  -- Trick: sum (Just 1) = 1, sum Nothing = 0
          pure $ Right ( Just totalLength
                       , sequenceConduits contents' >> pure () ) -- .| mapM_C (printDebug "[parseFormatC] doc")
    _ -> pure $ Left $ unpack $ intercalate "\n" $ pack <$> errs
  
parseFormatC _ _ _ = undefined

-- parseFormat :: FileType -> DB.ByteString -> IO (Either Prelude.String [HyperdataDocument])
-- parseFormat CsvGargV3 bs = pure $ parseCsv' $ DBL.fromStrict bs
-- parseFormat CsvHal    bs = pure $ parseHal' $ DBL.fromStrict bs
-- parseFormat RisPresse bs = do
--   docs <- mapM (toDoc RIS)
--           <$> snd
--           <$> enrichWith RisPresse
--           $ partitionEithers
--           $ [runParser'  RisPresse bs]
--   pure $ Right docs
-- parseFormat WOS bs = do
--   docs <- mapM (toDoc WOS)
--           <$> snd
--           <$> enrichWith WOS
--           $ partitionEithers
--           $ [runParser'  WOS bs]
--   pure $ Right docs
-- parseFormat ZIP bs = do
--   path <- emptySystemTempFile "parsed-zip"
--   DB.writeFile path bs
--   parsedZip <- withArchive path $ do
--     DM.keys <$> getEntries
--   pure $ Left $ "Not implemented for ZIP, parsedZip" <> show parsedZip
-- parseFormat _ _ = undefined

-- | Parse file into documents
-- TODO manage errors here
-- TODO: to debug maybe add the filepath in error message

parseFile :: FileType -> FileFormat -> FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseFile CsvHal    Plain p = parseHal p
parseFile CsvGargV3 Plain p = parseCsv p

parseFile RisPresse Plain p = do
  docs <- join $ mapM (toDoc RIS) <$> snd <$> enrichWith RisPresse <$> readFileWith RIS p
  pure $ Right docs

parseFile WOS       Plain p = do
  docs <- join $ mapM (toDoc WOS) <$> snd <$> enrichWith WOS       <$> readFileWith WOS p
  pure $ Right docs

parseFile ff        _ p = do
  docs <- join $ mapM (toDoc ff)  <$> snd <$> enrichWith ff        <$> readFileWith ff  p
  pure $ Right docs

toDoc :: FileType -> [(Text, Text)] -> IO HyperdataDocument
-- TODO use language for RIS
toDoc ff d = do
      -- let abstract = lookup "abstract" d
      let lang = EN -- maybe EN identity (join $ detectLangDefault <$> (fmap (DT.take 50) abstract))

      let dateToParse = DT.replace " " "" <$> lookup "PY" d  -- <> Just " " <> lookup "publication_date" d 
      -- printDebug "[G.C.T.C.Parsers] dateToParse" dateToParse
      (utcTime, (pub_year, pub_month, pub_day)) <- Date.dateSplit lang dateToParse

      let hd = HyperdataDocument { _hd_bdd = Just $ DT.pack $ show ff
                             , _hd_doi = lookup "doi" d
                             , _hd_url = lookup "URL" d
                             , _hd_uniqId = Nothing
                             , _hd_uniqIdBdd = Nothing
                             , _hd_page = Nothing
                             , _hd_title = lookup "title" d
                             , _hd_authors = lookup "authors" d
                             , _hd_institutes = lookup "institutes" d
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
      -- printDebug "[G.C.T.C.Parsers] HyperdataDocument" hd
      pure hd

enrichWith :: FileType
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith RisPresse = enrichWith' presseEnrich
enrichWith WOS       = enrichWith' (map (first WOS.keys))
enrichWith _         = enrichWith' identity


enrichWith' ::      ([(DB.ByteString, DB.ByteString)] -> [(DB.ByteString, DB.ByteString)])
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith' f = second (map both' . map f . concat)
  where
    both'   = map (both decodeUtf8)



readFileWith :: FileType -> FilePath
       -> IO ([ParseError], [[[(DB.ByteString, DB.ByteString)]]])
readFileWith format path = do
    files <- case takeExtension path of
              ".zip" -> openZip              path
              _      -> pure <$> clean <$> DB.readFile path
    partitionEithers <$> mapConcurrently (runParser format) files


-- | withParser:
-- According to the format of the text, choose the right parser.
-- TODO  withParser :: FileType -> Parser [Document]
withParser :: FileType -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS = WOS.parser
withParser RIS = RIS.parser
--withParser ODT = odtParser
--withParser XML = xmlParser
withParser _   = panic "[ERROR] Parser not implemented yet"

runParser :: FileType -> DB.ByteString
          -> IO (Either String [[(DB.ByteString, DB.ByteString)]])
runParser format text = pure $ runParser' format text

runParser' :: FileType -> DB.ByteString
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

-- 

splitOn :: NgramsType -> Maybe Text -> Text -> [Text]
splitOn Authors (Just "WOS") = (DT.splitOn "; ")
splitOn _ _                  = (DT.splitOn ", ")

