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

module Gargantext.Text.Parsers -- (parse, FileFormat(..))
    where

import System.FilePath (FilePath(), takeExtension)
import Codec.Archive.Zip (withArchive, getEntry, getEntries)

import Data.Either.Extra (partitionEithers)
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

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Text.Parsers.WOS (wosParser)
------------------------------------------------------------------------


type ParseError = String
type Field      = Text
type Document   = DM.Map Field Text

type FilesParsed = DM.Map FilePath FileParsed
data FileParsed  = FileParsed { _fileParsed_errors ::  Maybe ParseError
                              , _fileParsed_result :: [Document]
                              } deriving (Show)


-- | According to the format of Input file,
-- different parser are available.
data FileFormat = WOS        -- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODT        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :
--                             -- > http://chrisdone.com/posts/fast-haskell-c-parsing-xml

-- TODO: to debug maybe add the filepath in error message


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
-- According the format of the text, choosing the right parser.
-- TODO  withParser :: FileFormat -> Parser [Document]
withParser :: FileFormat -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS = wosParser
--withParser DOC = docParser
--withParser ODT = odtParser
--withParser XML = xmlParser
--withParser _   = error "[ERROR] Parser not implemented yet"

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


