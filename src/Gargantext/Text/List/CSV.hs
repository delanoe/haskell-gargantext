{-|
Module      : Gargantext.Text.List.CSV
Description : 
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gargantext.Text.List.CSV where

import GHC.Real (round)
import GHC.IO (FilePath)

import Control.Applicative
import Control.Monad (mzero)

import Data.Char (ord)
import Data.Csv
import Data.Either (Either(Left, Right))
import Data.Text (Text, pack, length, intercalate)
import qualified Data.ByteString.Lazy as BL

import Data.Vector (Vector)
import qualified Data.Vector as V

import Gargantext.Prelude hiding (length)
import Gargantext.Text.List.Types
------------------------------------------------------------------------

--csv2lists :: Vector CsvList -> Lists
--csv2lists v = V.foldl' (\e (CsvList listType label forms) -> insertLists lt label forms e) emptyLists v

------------------------------------------------------------------------
data CsvListType = CsvMap | CsvStop | CsvCandidate
  deriving (Read, Show, Eq)
------------------------------------------------------------------------
-- CSV List Main Configuration
csvListFieldDelimiter :: Char
csvListFieldDelimiter = '\t'

csvListFormsDelimiter :: Text
csvListFormsDelimiter = "|&|"
------------------------------------------------------------------------
data CsvList = CsvList
    { csvList_status :: !CsvListType
    , csvList_label  :: !Text
    , csvList_forms  :: !Text
    }
    deriving (Show)
------------------------------------------------------------------------
instance FromNamedRecord CsvList where
  parseNamedRecord r = CsvList <$> r .: "status"
                               <*> r .: "label"
                               <*> r .: "forms"

instance ToNamedRecord CsvList where
  toNamedRecord (CsvList s l f) =
    namedRecord [ "status" .= s
                , "label"  .= l
                , "forms"  .= f
                ]
------------------------------------------------------------------------
instance FromField CsvListType where
    parseField "map"  = pure CsvMap
    parseField "main" = pure CsvCandidate
    parseField "stop" = pure CsvStop
    parseField _      = mzero

instance ToField CsvListType where
    toField CsvMap       = "map"
    toField CsvCandidate = "main"
    toField CsvStop      = "stop"
------------------------------------------------------------------------
csvDecodeOptions :: DecodeOptions
csvDecodeOptions = (defaultDecodeOptions
                      {decDelimiter = fromIntegral $ ord csvListFieldDelimiter}
                    )

csvEncodeOptions :: EncodeOptions
csvEncodeOptions = ( defaultEncodeOptions 
                      {encDelimiter = fromIntegral $ ord csvListFieldDelimiter}
                    )
------------------------------------------------------------------------
fromCsvListFile :: FilePath -> IO (Header, Vector CsvList)
fromCsvListFile fp = do
    csvData <- BL.readFile fp
    case decodeByNameWith csvDecodeOptions csvData of
      Left e        -> panic (pack e)
      Right csvList -> pure csvList
------------------------------------------------------------------------
toCsvListFile :: FilePath -> (Header, Vector CsvList) -> IO ()
toCsvListFile fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith csvEncodeOptions h (V.toList vs)
------------------------------------------------------------------------
