{-|
Module      : Gargantext.Parsers
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

module Gargantext.Parsers -- (parse, FileFormat(..))
    where

import Gargantext.Prelude

import System.FilePath (takeExtension, FilePath())
import Data.Attoparsec.ByteString (parseOnly, Parser)
import qualified Data.ByteString as DB
import qualified Data.Map        as DM
import Data.Either.Extra (partitionEithers)
import Data.Ord()
import Data.Foldable (concat)
import Data.String()
import Data.Either.Extra(Either())

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
----
--import Control.Monad (join)
import Codec.Archive.Zip (withArchive, getEntry, getEntries)
import Path.IO (resolveFile')
------ import qualified Data.ByteString.Lazy as B
--import Control.Applicative ( (<$>) )
import Control.Concurrent.Async as CCA (mapConcurrently)

import Data.String (String())
import Gargantext.Parsers.WOS (wosParser)
---- import Gargantext.Parsers.XML (xmlParser)
---- import Gargantext.Parsers.DOC (docParser)
---- import Gargantext.Parsers.ODT (odtParser)

--import Gargantext.Types.Main (ErrorMessage(), Corpus)

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


--parse :: FileFormat -> FilePath -> IO ([ParseError], [[(Text, Text)]])
--parse format path = do
--    files <- case takeExtension path of
--              ".zip" -> openZip              path
--              _      -> pure <$> DB.readFile path
--    (as, bs) <- partitionEithers <$> mapConcurrently (runParser format) files
--    pure (as, map toText $ concat bs)
--      where
--        -- TODO : decode with bayesian inference on encodings
--        toText = map (\(a,b) -> (decodeUtf8 a, decodeUtf8 b))
--
--
---- | withParser:
---- According the format of the text, choosing the right parser.
---- TODO  withParser :: FileFormat -> Parser [Document]
--withParser :: FileFormat -> Parser [[(DB.ByteString, DB.ByteString)]]
--withParser WOS = wosParser
----withParser DOC = docParser
----withParser ODT = odtParser
----withParser XML = xmlParser
----withParser _   = error "[ERROR] Parser not implemented yet"
--
--runParser :: FileFormat -> DB.ByteString 
--          -> IO (Either String [[(DB.ByteString, DB.ByteString)]])
--runParser format text = pure $ parseOnly (withParser format) text
--
--openZip :: FilePath -> IO [DB.ByteString]
--openZip fp = do
--    path    <- resolveFile' fp
--    entries <- withArchive path (DM.keys <$> getEntries)
--    bs      <- mapConcurrently (\s -> withArchive path (getEntry s)) entries
--    pure bs


