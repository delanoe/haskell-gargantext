{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.CSV
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}


module Gargantext.Core.Text.Corpus.Parsers.CSV where

import Conduit
import Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Csv
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, length, intercalate)
import Data.Time.Segment (jour)
import qualified Data.Vector          as V
import Data.Vector (Vector)
import GHC.IO (FilePath)
import GHC.Word (Word8)

import qualified Prelude

import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (length)
import Gargantext.Core.Text
import Gargantext.Core.Text.Context

---------------------------------------------------------------
headerCsvGargV3 :: Header
headerCsvGargV3 =
  header [ "title"
         , "source"
         , "publication_year"
         , "publication_month"
         , "publication_day"
         , "abstract"
         , "authors"
         ]
---------------------------------------------------------------
data CsvGargV3 = CsvGargV3
    { d_docId             :: !Int
    , d_title             :: !Text
    , d_source            :: !Text
    , d_publication_year  :: !Int
    , d_publication_month :: !Int
    , d_publication_day   :: !Int
    , d_abstract          :: !Text
    , d_authors           :: !Text
    }
    deriving (Show)
---------------------------------------------------------------
-- | Doc 2 HyperdataDocument
toDoc :: CsvGargV3 -> HyperdataDocument
toDoc (CsvGargV3 did dt _ dpy dpm dpd dab dau) =
  HyperdataDocument { _hd_bdd = Just "CSV"
                    , _hd_doi = Just . pack . show $ did
                    , _hd_url = Nothing
                    , _hd_uniqId = Nothing
                    , _hd_uniqIdBdd = Nothing
                    , _hd_page = Nothing
                    , _hd_title = Just dt
                    , _hd_authors = Nothing
                    , _hd_institutes = Just dau
                    , _hd_source = Just dab
                    , _hd_abstract = Nothing
                    , _hd_publication_date = Nothing
                    , _hd_publication_year = Just dpy
                    , _hd_publication_month = Just dpm
                    , _hd_publication_day = Just dpd
                    , _hd_publication_hour = Nothing
                    , _hd_publication_minute = Nothing
                    , _hd_publication_second = Nothing
                    , _hd_language_iso2 = Nothing }

---------------------------------------------------------------
-- | Types Conversions
toDocs :: Vector CsvDoc -> [CsvGargV3]
toDocs v = V.toList
         $ V.zipWith (\nId (CsvDoc { .. }) -- (CsvDoc t s mPy pm pd abst auth)
                       -> CsvGargV3 { d_docId = nId
                                    , d_title = csv_title
                                    , d_source = csv_source
                                    , d_publication_year = fromMIntOrDec defaultYear csv_publication_year
                                    , d_publication_month = fromMaybe defaultMonth csv_publication_month
                                    , d_publication_day = fromMaybe defaultDay csv_publication_day
                                    , d_abstract = csv_abstract
                                    , d_authors = csv_authors })
                       (V.enumFromN 1 (V.length v'')) v''
          where
            v'' = V.foldl (\v' sep -> V.concatMap (splitDoc (docsSize v') sep) v') v seps
            seps= (V.fromList [Paragraphs 1, Sentences 3, Chars 3])

---------------------------------------------------------------
fromDocs :: Vector CsvGargV3 -> Vector CsvDoc
fromDocs docs = V.map fromDocs' docs
  where
    fromDocs' (CsvGargV3 { .. }) = CsvDoc { csv_title = d_title
                                          , csv_source = d_source
                                          , csv_publication_year = Just $ IntOrDec d_publication_year
                                          , csv_publication_month = Just d_publication_month
                                          , csv_publication_day = Just d_publication_day
                                          , csv_abstract = d_abstract
                                          , csv_authors = d_authors }

---------------------------------------------------------------
-- | Split a document in its context
-- TODO adapt the size of the paragraph according to the corpus average
splitDoc :: Mean -> SplitContext -> CsvDoc -> Vector CsvDoc
splitDoc m splt doc = let docSize = (length $ csv_abstract doc) in
                          if docSize > 1000
                            then
                              if (mod (round m) docSize) >= 10
                                then
                                  splitDoc' splt doc
                                else
                                  V.fromList [doc]
                            else
                              V.fromList [doc]
  where
    splitDoc' :: SplitContext -> CsvDoc -> Vector CsvDoc
    splitDoc' contextSize (CsvDoc { .. }) = V.fromList $ [firstDoc] <> nextDocs
        where
          firstDoc = CsvDoc { csv_abstract = firstAbstract, .. }
          firstAbstract = head' "splitDoc'1" abstracts

          nextDocs = map (\txt -> CsvDoc { csv_title = head' "splitDoc'2" $ sentences txt
                                         , csv_abstract = unsentences $ tail' "splitDoc'1" $ sentences txt
                                         , .. }
                          ) (tail' "splitDoc'2" abstracts)

          abstracts    = (splitBy $ contextSize) csv_abstract

---------------------------------------------------------------
---------------------------------------------------------------
type Mean = Double

docsSize :: Vector CsvDoc -> Mean
docsSize csvDoc = mean ls
  where
    ls = V.toList $ V.map (fromIntegral . length . csv_abstract) csvDoc


---------------------------------------------------------------
newtype IntOrDec = IntOrDec Int
  deriving (Show, Eq, Read)
unIntOrDec :: IntOrDec -> Int
unIntOrDec (IntOrDec i) = i
instance FromField IntOrDec where
  parseField s = case runParser (parseField s :: Parser Int) of
    Left _err -> IntOrDec <$> Prelude.floor <$> (parseField s :: Parser Double)
    Right n   -> pure $ IntOrDec n
instance ToField IntOrDec where
  toField (IntOrDec i) = toField i

fromMIntOrDec :: Int -> Maybe IntOrDec -> Int
fromMIntOrDec default' mVal = unIntOrDec $ fromMaybe (IntOrDec default') mVal
defaultYear :: Int
defaultYear = 1973
defaultMonth :: Int
defaultMonth = 1
defaultDay :: Int
defaultDay = 1

data CsvDoc = CsvDoc
    { csv_title             :: !Text
    , csv_source            :: !Text
    , csv_publication_year  :: !(Maybe IntOrDec)
    , csv_publication_month :: !(Maybe Int)
    , csv_publication_day   :: !(Maybe Int)
    , csv_abstract          :: !Text
    , csv_authors           :: !Text
    }
    deriving (Show)

instance FromNamedRecord CsvDoc where
  parseNamedRecord r = do
    csv_title <- r .: "title" <|> r .: "Title"
    csv_source <- r .: "source" <|> r .: "Source"
    csv_publication_year <- r .: "publication_year" <|> r .: "Publication Year"
    csv_publication_month <- r .: "publication_month" <|> r .: "Publication Month"
    csv_publication_day <- r .: "publication_day" <|> r .: "Publication Day"
    csv_abstract <- r .: "abstract" <|> r .: "Abstract"
    csv_authors <- r .: "authors" <|> r .: "Authors"
    pure $ CsvDoc { .. }

instance ToNamedRecord CsvDoc where
  toNamedRecord (CsvDoc{ .. }) =
    namedRecord [ "title"             .= csv_title
                , "source"            .= csv_source
                , "publication_year"  .= csv_publication_year
                , "publication_month" .= csv_publication_month
                , "publication_day"   .= csv_publication_day
                , "abstract"          .= csv_abstract
                , "authors"           .= csv_authors
                ]

hyperdataDocument2csvDoc :: HyperdataDocument -> CsvDoc
hyperdataDocument2csvDoc h = CsvDoc { csv_title = m $ _hd_title h
                                    , csv_source = m $ _hd_source h
                                    , csv_publication_year = Just $ IntOrDec $ mI $ _hd_publication_year h
                                    , csv_publication_month = Just $ mI $ _hd_publication_month h
                                    , csv_publication_day = Just $ mI $ _hd_publication_day   h
                                    , csv_abstract = m $ _hd_abstract h
                                    , csv_authors = m $ _hd_authors h }

  where
    m = maybe "" identity
    mI = maybe 0 identity


data Delimiter = Tab | Comma

csvDecodeOptions :: Delimiter -> DecodeOptions
csvDecodeOptions d = defaultDecodeOptions {decDelimiter = delimiter d}

csvEncodeOptions :: Delimiter -> EncodeOptions
csvEncodeOptions d = defaultEncodeOptions {encDelimiter = delimiter d}

delimiter :: Delimiter -> Word8
delimiter Tab   = fromIntegral $ ord '\t'
delimiter Comma = fromIntegral $ ord ','
------------------------------------------------------------------------
------------------------------------------------------------------------
readCsvOn' :: [CsvDoc -> Text] -> FilePath -> IO (Either Prelude.String [Text])
readCsvOn' fields fp = do
  r <- readCSVFile fp
  pure $ ( V.toList
          . V.map (\l -> intercalate (pack " ") $ map (\field -> field l) fields)
          . snd ) <$> r

------------------------------------------------------------------------

readFileLazy :: (FromNamedRecord a) => proxy a -> Delimiter -> FilePath -> IO (Either Prelude.String (Header, Vector a))
readFileLazy d f = fmap (readByteStringLazy d f) . BL.readFile

readFileStrict :: (FromNamedRecord a)
               => proxy a
               -> Delimiter
               -> FilePath
               -> IO (Either Prelude.String (Header, Vector a))
readFileStrict d f = fmap (readByteStringStrict d f) . BS.readFile

readByteStringLazy :: (FromNamedRecord a)
                   => proxy a
                   -> Delimiter
                   -> BL.ByteString
                   -> Either Prelude.String (Header, Vector a)
readByteStringLazy _f d bs = decodeByNameWith (csvDecodeOptions d) bs

readByteStringStrict :: (FromNamedRecord a)
                     => proxy a
                     -> Delimiter
                     -> BS.ByteString
                     -> Either Prelude.String (Header, Vector a)
readByteStringStrict d ff = (readByteStringLazy d ff) . BL.fromStrict

------------------------------------------------------------------------
-- | TODO use readFileLazy
readCSVFile :: FilePath -> IO (Either Prelude.String (Header, Vector CsvDoc))
readCSVFile fp = do
  result <- fmap (readCsvLazyBS Comma) $ BL.readFile fp
  case result of
    Left _err -> fmap (readCsvLazyBS Tab) $ BL.readFile fp
    Right res -> pure $ Right res



-- | TODO use readByteStringLazy
readCsvLazyBS :: Delimiter -> BL.ByteString -> Either Prelude.String (Header, Vector CsvDoc)
readCsvLazyBS d bs = decodeByNameWith (csvDecodeOptions d) bs

------------------------------------------------------------------------
-- | TODO use readFileLazy
readCsvHal :: FilePath -> IO (Either Prelude.String (Header, Vector CsvHal))
readCsvHal = fmap readCsvHalLazyBS . BL.readFile

-- | TODO use readByteStringLazy
readCsvHalLazyBS :: BL.ByteString -> Either Prelude.String (Header, Vector CsvHal)
readCsvHalLazyBS bs = decodeByNameWith (csvDecodeOptions Tab) bs

readCsvHalBSStrict :: BS.ByteString -> Either Prelude.String (Header, Vector CsvHal)
readCsvHalBSStrict = readCsvHalLazyBS . BL.fromStrict

------------------------------------------------------------------------
writeFile :: FilePath -> (Header, Vector CsvDoc) -> IO ()
writeFile fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith (csvEncodeOptions Tab) h (V.toList vs)

writeDocs2Csv :: FilePath -> [HyperdataDocument] -> IO ()
writeDocs2Csv fp hs = BL.writeFile fp $ hyperdataDocument2csv hs

hyperdataDocument2csv :: [HyperdataDocument] -> BL.ByteString
hyperdataDocument2csv hs = encodeByNameWith (csvEncodeOptions Tab) headerCsvGargV3 (map hyperdataDocument2csvDoc hs)

------------------------------------------------------------------------
-- Hal Format
data CsvHal = CsvHal
    { csvHal_title  :: !Text
    , csvHal_source :: !Text
    , csvHal_publication_year  :: !Integer
    , csvHal_publication_month :: !Int
    , csvHal_publication_day   :: !Int
    , csvHal_abstract          :: !Text
    , csvHal_authors           :: !Text

    , csvHal_url               :: !Text
    , csvHal_isbn_s            :: !Text
    , csvHal_issue_s           :: !Text
    , csvHal_journalPublisher_s:: !Text
    , csvHal_language_s        :: !Text

    , csvHal_doiId_s           :: !Text
    , csvHal_authId_i          :: !Text
    , csvHal_instStructId_i    :: !Text
    , csvHal_deptStructId_i    :: !Text
    , csvHal_labStructId_i     :: !Text

    , csvHal_rteamStructId_i   :: !Text
    , csvHal_docType_s         :: !Text
    }
    deriving (Show)

instance FromNamedRecord CsvHal where
  parseNamedRecord r = do
    csvHal_title <- r .: "title"
    csvHal_source <- r .: "source"
    csvHal_publication_year <- r .: "publication_year"
    csvHal_publication_month <- r .: "publication_month"
    csvHal_publication_day <- r .: "publication_day"
    csvHal_abstract <- r .: "abstract"
    csvHal_authors <- r .: "authors"
    csvHal_url <- r .: "url"
    csvHal_isbn_s <- r .: "isbn_s"
    csvHal_issue_s <- r .: "issue_s"
    csvHal_journalPublisher_s <- r .: "journalPublisher_s"
    csvHal_language_s <- r .: "language_s"
    csvHal_doiId_s <- r .: "doiId_s"
    csvHal_authId_i <- r .: "authId_i"
    csvHal_instStructId_i <- r .: "instStructId_i"
    csvHal_deptStructId_i <- r .: "deptStructId_i"
    csvHal_labStructId_i <- r .: "labStructId_i"
    csvHal_rteamStructId_i <- r .: "rteamStructId_i"
    csvHal_docType_s <- r .: "docType_s"
    pure $ CsvHal { .. }

instance ToNamedRecord CsvHal where
  --toNamedRecord (CsvHal t s py  pm pd abst aut  url isbn iss j lang  doi auth inst dept lab team doct) =
  toNamedRecord (CsvHal { .. }) =
    namedRecord [ "title"  .= csvHal_title
                , "source" .= csvHal_source

                , "publication_year"  .= csvHal_publication_year
                , "publication_month" .= csvHal_publication_month
                , "publication_day"   .= csvHal_publication_day

                , "abstract"          .= csvHal_abstract
                , "authors"           .= csvHal_authors

                , "url"                .= csvHal_url
                , "isbn_s"             .= csvHal_isbn_s
                , "issue_s"            .= csvHal_issue_s
                , "journalPublisher_s" .= csvHal_journalPublisher_s
                , "language_s"         .= csvHal_language_s

                , "doiId_s"            .= csvHal_doiId_s
                , "authId_i"           .= csvHal_authId_i
                , "instStructId_i"     .= csvHal_instStructId_i
                , "deptStructId_i"     .= csvHal_deptStructId_i
                , "labStructId_i"      .= csvHal_labStructId_i
 
                , "rteamStructId_i"    .= csvHal_rteamStructId_i
                , "docType_s"          .= csvHal_docType_s
               ]

csvHal2doc :: CsvHal -> HyperdataDocument
csvHal2doc (CsvHal { .. }) =
  HyperdataDocument { _hd_bdd = Just "CsvHal"
                    , _hd_doi = Just csvHal_doiId_s
                    , _hd_url = Just csvHal_url
                    , _hd_uniqId = Nothing
                    , _hd_uniqIdBdd = Nothing
                    , _hd_page = Nothing
                    , _hd_title = Just csvHal_title
                    , _hd_authors = Just csvHal_authors
                    , _hd_institutes = Just csvHal_instStructId_i
                    , _hd_source = Just csvHal_source
                    , _hd_abstract = Just csvHal_abstract
                    , _hd_publication_date = Just $ pack . show $ jour csvHal_publication_year
                                                                      csvHal_publication_month
                                                                      csvHal_publication_day
                    , _hd_publication_year = Just $ fromIntegral csvHal_publication_year
                    , _hd_publication_month = Just csvHal_publication_month
                    , _hd_publication_day = Just csvHal_publication_day
                    , _hd_publication_hour = Nothing
                    , _hd_publication_minute = Nothing
                    , _hd_publication_second = Nothing
                    , _hd_language_iso2 = Nothing }


csv2doc :: CsvDoc -> HyperdataDocument
csv2doc (CsvDoc { .. })
  = HyperdataDocument { _hd_bdd = Just "CsvHal"
                      , _hd_doi = Nothing
                      , _hd_url = Nothing
                      , _hd_uniqId = Nothing
                      , _hd_uniqIdBdd = Nothing
                      , _hd_page = Nothing
                      , _hd_title = Just csv_title
                      , _hd_authors = Just csv_authors
                      , _hd_institutes = Nothing
                      , _hd_source = Just csv_source
                      , _hd_abstract = Just csv_abstract
                      , _hd_publication_date = Just $ pack . show $ jour (fromIntegral pubYear)
                                                                         pubMonth
                                                                         pubDay
                      , _hd_publication_year = Just pubYear
                      , _hd_publication_month = Just pubMonth
                      , _hd_publication_day = Just pubDay
                      , _hd_publication_hour = Nothing
                      , _hd_publication_minute = Nothing
                      , _hd_publication_second = Nothing
                      , _hd_language_iso2 = Nothing }
  where
    pubYear = fromMIntOrDec defaultYear csv_publication_year
    pubMonth = fromMaybe defaultMonth csv_publication_month
    pubDay = fromMaybe defaultDay csv_publication_day

------------------------------------------------------------------------
parseHal :: FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseHal fp = do
  r <- readCsvHal fp
  pure $ (V.toList . V.map csvHal2doc . snd) <$> r

parseHal' :: BL.ByteString -> Either Prelude.String [HyperdataDocument]
parseHal' bs = (V.toList . V.map csvHal2doc . snd) <$> readCsvHalLazyBS bs

------------------------------------------------------------------------

parseCsv :: FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseCsv fp = fmap (V.toList . V.map csv2doc . snd) <$> readCSVFile fp

{-
parseCsv' ::  BL.ByteString -> Either Prelude.String [HyperdataDocument]
parseCsv' bs = (V.toList . V.map csv2doc . snd) <$> readCsvLazyBS Comma bs
-}

parseCsv' :: BL.ByteString -> Either Prelude.String [HyperdataDocument]
parseCsv' bs = do
  let
    result = case readCsvLazyBS Comma bs of
      Left  _err -> readCsvLazyBS Tab bs
      Right res -> Right res
  (V.toList . V.map csv2doc . snd) <$> result

parseCsvC :: BL.ByteString
          -> Either Prelude.String (Maybe Integer, ConduitT () HyperdataDocument Identity ())
parseCsvC bs = do
  let
    result = case readCsvLazyBS Comma bs of
      Left  _err -> readCsvLazyBS Tab bs
      Right res -> Right res
  case result of
    Left err -> Left err
    Right r -> Right $ (Just $ Prelude.fromIntegral $ Prelude.length $ snd r, (yieldMany $ snd r) .| mapC csv2doc)

------------------------------------------------------------------------
-- Csv v3 weighted for phylo

data Csv' = Csv'
      { csv'_title             :: !Text
      , csv'_source            :: !Text
      , csv'_publication_year  :: !Int
      , csv'_publication_month :: !Int
      , csv'_publication_day   :: !Int
      , csv'_abstract          :: !Text
      , csv'_authors           :: !Text
      , csv'_weight            :: !Double } deriving (Show)


instance FromNamedRecord Csv' where
  parseNamedRecord r = do
    csv'_title <- r .: "title"
    csv'_source <- r .: "source"
    csv'_publication_year <- r .: "publication_year"
    csv'_publication_month <- r .: "publication_month"
    csv'_publication_day <- r .: "publication_day"
    csv'_abstract <- r .: "abstract"
    csv'_authors <- r .: "authors"
    csv'_weight <- r .: "weight"
    pure $ Csv' { .. }

readWeightedCsv :: FilePath -> IO (Header, Vector Csv')
readWeightedCsv fp =
  fmap (\bs ->
    case decodeByNameWith (csvDecodeOptions Tab) bs of
      Left e       -> panic (pack e)
      Right corpus -> corpus
    ) $ BL.readFile fp
