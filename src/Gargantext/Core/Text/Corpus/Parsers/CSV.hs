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

import Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Csv
import Data.Either (Either(..))
import Data.Text (Text, pack, length, intercalate)
import Data.Time.Segment (jour)
import qualified Data.Vector          as V
import Data.Vector (Vector)
import GHC.IO (FilePath)
import GHC.Word (Word8)

import qualified Prelude as Prelude

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
  HyperdataDocument (Just "CSV")
                    (Just . pack . show $ did)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (Just dt)
                    Nothing
                    (Just dau)
                    (Just dab)
                    (Nothing)
                    Nothing
                    (Just dpy)
                    (Just dpm)
                    (Just dpd)
                    Nothing
                    Nothing
                    Nothing
                    Nothing

---------------------------------------------------------------
-- | Types Conversions
toDocs :: Vector CsvDoc -> [CsvGargV3]
toDocs v = V.toList
         $ V.zipWith (\nId (CsvDoc t s py pm pd abst auth)
                       -> CsvGargV3 nId t s py pm pd abst auth )
                       (V.enumFromN 1 (V.length v'')) v''
          where
            v'' = V.foldl (\v' sep -> V.concatMap (splitDoc (docsSize v') sep) v') v seps
            seps= (V.fromList [Paragraphs 1, Sentences 3, Chars 3])

---------------------------------------------------------------
fromDocs :: Vector CsvGargV3 -> Vector CsvDoc
fromDocs docs = V.map fromDocs' docs
  where
    fromDocs' (CsvGargV3 _ t s py pm pd abst auth) = (CsvDoc t s py pm pd abst auth)

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
    splitDoc' contextSize (CsvDoc t s py pm pd abst auth) = V.fromList $ [firstDoc] <> nextDocs
        where
          firstDoc = CsvDoc t s py pm pd firstAbstract auth
          firstAbstract = head' "splitDoc'1" abstracts

          nextDocs = map (\txt -> CsvDoc
                                    (head' "splitDoc'2" $ sentences txt)
                                    s py pm pd 
                                    (unsentences $ tail' "splitDoc'1" $ sentences txt)
                                    auth
                          ) (tail' "splitDoc'2" abstracts)

          abstracts    = (splitBy $ contextSize) abst

---------------------------------------------------------------
---------------------------------------------------------------
type Mean = Double

docsSize :: Vector CsvDoc -> Mean
docsSize csvDoc = mean ls
  where
    ls = V.toList $ V.map (fromIntegral . length . csv_abstract) csvDoc


---------------------------------------------------------------
data CsvDoc = CsvDoc
    { csv_title  :: !Text
    , csv_source :: !Text
    , csv_publication_year  :: !Int
    , csv_publication_month :: !Int
    , csv_publication_day   :: !Int
    , csv_abstract          :: !Text
    , csv_authors           :: !Text
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

hyperdataDocument2csvDoc :: HyperdataDocument -> CsvDoc
hyperdataDocument2csvDoc h = CsvDoc (m  $ _hd_title h)
                                    (m  $ _hd_source h)
                                    (mI $ _hd_publication_year h)
                                    (mI $ _hd_publication_month h)
                                    (mI $ _hd_publication_day   h)
                                    (m  $ _hd_abstract h)
                                    (m  $ _hd_authors h)

  where
    m = maybe "" identity
    mI = maybe 0 identity


csvDecodeOptions :: DecodeOptions
csvDecodeOptions = defaultDecodeOptions {decDelimiter = delimiter}

csvEncodeOptions :: EncodeOptions
csvEncodeOptions = defaultEncodeOptions {encDelimiter = delimiter}

delimiter :: Word8
delimiter = fromIntegral $ ord '\t'
------------------------------------------------------------------------
------------------------------------------------------------------------
readCsvOn' :: [CsvDoc -> Text] -> FilePath -> IO (Either Prelude.String [Text])
readCsvOn' fields fp = do
  r <- readFile fp
  pure $ ( V.toList
          . V.map (\l -> intercalate (pack " ") $ map (\field -> field l) fields)
          . snd ) <$> r

------------------------------------------------------------------------

readFileLazy :: (FromNamedRecord a) => proxy a -> FilePath -> IO (Either Prelude.String (Header, Vector a))
readFileLazy f = fmap (readByteStringLazy f) . BL.readFile

readFileStrict :: (FromNamedRecord a) => proxy a -> FilePath -> IO (Either Prelude.String (Header, Vector a))
readFileStrict f = fmap (readByteStringStrict f) . BS.readFile

readByteStringLazy :: (FromNamedRecord a) => proxy a -> BL.ByteString -> Either Prelude.String (Header, Vector a)
readByteStringLazy _f bs = decodeByNameWith csvDecodeOptions bs

readByteStringStrict :: (FromNamedRecord a) => proxy a -> BS.ByteString -> Either Prelude.String (Header, Vector a)
readByteStringStrict ff = (readByteStringLazy ff) . BL.fromStrict

------------------------------------------------------------------------
-- | TODO use readFileLazy
readFile :: FilePath -> IO (Either Prelude.String (Header, Vector CsvDoc))
readFile = fmap readCsvLazyBS . BL.readFile


-- | TODO use readByteStringLazy
readCsvLazyBS :: BL.ByteString -> Either Prelude.String (Header, Vector CsvDoc)
readCsvLazyBS bs = decodeByNameWith csvDecodeOptions bs

------------------------------------------------------------------------
-- | TODO use readFileLazy
readCsvHal :: FilePath -> IO (Either Prelude.String (Header, Vector CsvHal))
readCsvHal = fmap readCsvHalLazyBS . BL.readFile

-- | TODO use readByteStringLazy
readCsvHalLazyBS :: BL.ByteString -> Either Prelude.String (Header, Vector CsvHal)
readCsvHalLazyBS bs = decodeByNameWith csvDecodeOptions bs

readCsvHalBSStrict :: BS.ByteString -> Either Prelude.String (Header, Vector CsvHal)
readCsvHalBSStrict = readCsvHalLazyBS . BL.fromStrict

------------------------------------------------------------------------
writeFile :: FilePath -> (Header, Vector CsvDoc) -> IO ()
writeFile fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith csvEncodeOptions h (V.toList vs)

writeDocs2Csv :: FilePath -> [HyperdataDocument] -> IO ()
writeDocs2Csv fp hs = BL.writeFile fp $ hyperdataDocument2csv hs

hyperdataDocument2csv :: [HyperdataDocument] -> BL.ByteString
hyperdataDocument2csv hs = encodeByNameWith csvEncodeOptions headerCsvGargV3 (map hyperdataDocument2csvDoc hs)

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
  parseNamedRecord r = CsvHal <$> r .: "title"
                              <*> r .: "source"
                              <*> r .: "publication_year"
                              <*> r .: "publication_month"
                              <*> r .: "publication_day"
                              <*> r .: "abstract"
                              <*> r .: "authors"

                              <*> r .: "url"
                              <*> r .: "isbn_s"
                              <*> r .: "issue_s"
                              <*> r .: "journalPublisher_s"
                              <*> r .: "language_s"

                              <*> r .: "doiId_s"
                              <*> r .: "authId_i"
                              <*> r .: "instStructId_i"
                              <*> r .: "deptStructId_i"
                              <*> r .: "labStructId_i"

                              <*> r .: "rteamStructId_i"
                              <*> r .: "docType_s"

instance ToNamedRecord CsvHal where
  toNamedRecord (CsvHal t s py  pm pd abst aut  url isbn iss j lang  doi auth inst dept lab team doct) = 
    namedRecord [ "title"  .= t
                , "source" .= s

                , "publication_year"  .= py
                , "publication_month" .= pm
                , "publication_day"   .= pd

                , "abstract"          .= abst
                , "authors"           .= aut

                , "url"                .= url
                , "isbn_s"             .= isbn
                , "issue_s"            .= iss
                , "journalPublisher_s" .= j
                , "language_s"         .= lang

                , "doiId_s"            .= doi
                , "authId_i"           .= auth
                , "instStructId_i"     .= inst
                , "deptStructId_i"     .= dept
                , "labStructId_i"      .= lab
 
                , "rteamStructId_i"    .= team
                , "docType_s"          .= doct
               ]

csvHal2doc :: CsvHal -> HyperdataDocument
csvHal2doc (CsvHal title source
       pub_year pub_month pub_day
       abstract authors
       url _ _ _ _
       doi _ inst _ _
       _ _ ) = HyperdataDocument (Just "CsvHal")
                               (Just doi)
                               (Just url)
                               Nothing
                               Nothing
                               Nothing
                               (Just title)
                               (Just authors)
                               (Just inst)
                               (Just source)
                               (Just abstract)
                               (Just $ pack . show $ jour pub_year pub_month pub_day)
                               (Just $ fromIntegral pub_year)
                               (Just pub_month)
                               (Just pub_day)
                               Nothing
                               Nothing
                               Nothing
                               Nothing


csv2doc :: CsvDoc -> HyperdataDocument
csv2doc (CsvDoc title source
       pub_year pub_month pub_day
       abstract authors ) = HyperdataDocument (Just "CsvHal")
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               (Just title)
                               (Just authors)
                               Nothing
                               (Just source)
                               (Just abstract)
                               (Just $ pack . show $ jour (fromIntegral pub_year) pub_month pub_day)
                               (Just $ fromIntegral pub_year)
                               (Just pub_month)
                               (Just pub_day)
                               Nothing
                               Nothing
                               Nothing
                               Nothing

------------------------------------------------------------------------
parseHal :: FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseHal fp = do
  r <- readCsvHal fp
  pure $ (V.toList . V.map csvHal2doc . snd) <$> r

parseHal' :: BL.ByteString -> Either Prelude.String [HyperdataDocument]
parseHal' bs = (V.toList . V.map csvHal2doc . snd) <$> readCsvHalLazyBS bs

------------------------------------------------------------------------
parseCsv :: FilePath -> IO (Either Prelude.String [HyperdataDocument])
parseCsv fp = do
  r <- readFile fp
  pure $ (V.toList . V.map csv2doc . snd) <$> r

parseCsv' :: BL.ByteString -> Either Prelude.String [HyperdataDocument]
parseCsv' bs = (V.toList . V.map csv2doc . snd) <$> readCsvLazyBS bs

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
  parseNamedRecord r = Csv' <$> r .: "title"
                            <*> r .: "source"
                            <*> r .: "publication_year"
                            <*> r .: "publication_month"
                            <*> r .: "publication_day"
                            <*> r .: "abstract"
                            <*> r .: "authors"
                            <*> r .: "weight"   

readWeightedCsv :: FilePath -> IO (Header, Vector Csv')
readWeightedCsv fp = 
  fmap (\bs -> 
    case decodeByNameWith csvDecodeOptions bs of
      Left e       -> panic (pack e)
      Right corpus -> corpus
    ) $ BL.readFile fp
