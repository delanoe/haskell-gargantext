{-|
Module      : Gargantext.Text.Parsers.CSV
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric      #-}

module Gargantext.Text.Parsers.CSV where

import GHC.Generics (Generic)
import GHC.IO (FilePath)
import Data.Either (Either(Left, Right))
import Data.Text (Text)
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Text (pack)

import Data.Char (ord)
import Gargantext.Prelude

data CsvDoc = CsvDoc
    { title  :: !Text
    , source :: !Text
    , publication_year  :: !Int
    , publication_month :: !Int
    , publication_day   :: !Int
    , abstract          :: !Text
    , authors           :: !Text
    }
    deriving (Show, Generic)

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
csvDecodeOptions = (defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'} )

csvEncodeOptions :: EncodeOptions
csvEncodeOptions = (defaultEncodeOptions {encDelimiter = fromIntegral $ ord '\t'} )


readCsv :: FilePath -> IO (Header, V.Vector CsvDoc)
readCsv fp = do
    csvData <- BL.readFile fp
    case decodeByNameWith csvDecodeOptions csvData of
      Left e    -> panic (pack e)
      Right csvDocs -> pure csvDocs

writeCsv :: FilePath -> (Header, V.Vector CsvDoc) -> IO ()
writeCsv fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith csvEncodeOptions h (V.toList vs)

