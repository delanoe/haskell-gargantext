{-|
Module      : Data.Gargantext.Parsers
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

module Data.Gargantext.Parsers -- (parse, FileFormat(..))
    where

import System.FilePath (takeExtension)
import Data.Attoparsec.ByteString (parseOnly, Parser)
import Data.ByteString as DB
import Data.Map                    as DM
----import Data.Either.Extra(Either(..))
----
--import Control.Monad (join)
import Codec.Archive.Zip
import Path.IO (resolveFile')
------ import qualified Data.ByteString.Lazy as B
--import Control.Applicative ( (<$>) )
import Control.Concurrent.Async as CCA (mapConcurrently)


import Data.Gargantext.Parsers.WOS (wosParser)
---- import Data.Gargantext.Parsers.XML (xmlParser)
---- import Data.Gargantext.Parsers.DOC (docParser)
---- import Data.Gargantext.Parsers.ODS (odsParser)

--import Data.Gargantext.Prelude (pm)
--import Data.Gargantext.Types.Main (ErrorMessage(), Corpus)


-- | According to the format of Input file,
-- different parser are available.
data FileFormat = WOS        -- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODS        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :
--                             -- > http://chrisdone.com/posts/fast-haskell-c-parsing-xml

---- | withParser:
---- According the format of the text, choosing the right parser.

--withParser :: FileFormat -> ByteString -> IO Corpus
withParser :: FileFormat -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS = wosParser
--withParser DOC = docParser
--withParser ODS = odsParser
--withParser XML = xmlParser
--withParser _   = error "[ERROR] Parser not implemented yet"

runParser :: FileFormat -> DB.ByteString 
          -> IO (Either String [[(DB.ByteString, DB.ByteString)]])
runParser format text = pure $ parseOnly (withParser format) text

openZip :: FilePath -> IO [DB.ByteString]
openZip fp = do
    path    <- resolveFile' fp
    entries <- withArchive path (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive path (getEntry s)) entries
    pure bs

parse :: FileFormat -> FilePath 
      -> IO [Either String [[(DB.ByteString, DB.ByteString)]]]
parse format path = do
    files <- case takeExtension path of
              ".zip" -> openZip              path
              _      -> pure <$> DB.readFile path
    mapConcurrently (runParser format) files


