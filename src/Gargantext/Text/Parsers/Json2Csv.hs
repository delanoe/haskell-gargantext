{-|
Module      : Gargantext.Text.Parsers.Json2Csv
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Json parser to export towoard CSV GargV3 format.
(Export from the Patent Database.)

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Text.Parsers.Json2Csv (json2csv, readPatents)
  where

import Prelude (read)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy (readFile)
import Data.Text (Text, unpack)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude
import System.IO (FilePath)
import Gargantext.Text.Parsers.CSV (CsvDoc(..), writeCsv, headerCsvGargV3)
import Data.Vector (fromList)

data Patent = Patent { _patent_title :: Text
                     , _patent_abstract :: Text
                     , _patent_year     :: Text
                     , _patent_id       :: Text
 } deriving (Show)

$(deriveJSON (unPrefix "_patent_") ''Patent)

readPatents :: FilePath -> IO (Maybe [Patent])
readPatents fp = decode <$> readFile fp

type FilePathIn  = FilePath
type FilePathOut = FilePath

json2csv :: FilePathIn -> FilePathOut -> IO ()
json2csv fin fout = do
  patents <- maybe (panic "json2csv error") identity <$> readPatents fin
  writeCsv fout (headerCsvGargV3, fromList $ map patent2csvDoc patents)

patent2csvDoc :: Patent -> CsvDoc
patent2csvDoc (Patent title abstract year _) =
  CsvDoc title "Source" (read (unpack year)) 1 1 abstract "Authors"





