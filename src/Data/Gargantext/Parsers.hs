{-|
Module      : Data.Gargantext.Parsers
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : alexandre.delanoe@iscpif.fr
Stability   : experimental
Portability : POSIX

Gargantext enables analyzing semi-structured text that should be parsed
in order to be analyzed.

The parsers suppose, we know the format of the Text (TextFormat data
type) according which the right parser is chosen among the list of
available parsers.

This module mainly describe how to add a new parser to Gargantext,
please follow the types.
-}


module Data.Gargantext.Parsers ( module Data.Gargantext.Parsers.WOS
                               --, module Data.Gargantext.Parsers.XML
                               --, module Data.Gargantext.Parsers.DOC
                               --, module Data.Gargantext.Parsers.ODS
                               )
    where


import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Map                    as DM
import Data.Either.Extra(Either(..))

import Control.Monad (join)
import Codec.Archive.Zip
import Path.IO (resolveFile')
-- import qualified Data.ByteString.Lazy as B
import Control.Applicative ( (<$>) )



import Control.Concurrent.Async as CCA (mapConcurrently)


import Data.Gargantext.Parsers.WOS (wosParser)
-- import Data.Gargantext.Parsers.XML (xmlParser)
-- import Data.Gargantext.Parsers.DOC (docParser)
-- import Data.Gargantext.Parsers.ODS (odsParser)

import Data.Gargantext.Prelude
import Data.Gargantext.Types.Main (ErrorMessage(), GargParser(), Corpus)


-- | According to the format of Input file,
-- different parser are available.
data FileFormat = WOS        -- Implemented (ISI Format)
                | XML        -- Not Implemented / see :
                             -- > http://chrisdone.com/posts/fast-haskell-c-parsing-xml
                | DOC        -- Not Implemented / import Pandoc
                | ODS        -- Not Implemented / import Pandoc
                | PDF        -- Not Implemented / pdftotext and import Pandoc ?



-- | withParser:
-- According the format of the text, choosing the right parser.
withParser :: FileFormat -> GargParser
withParser WOS = wosParser
--withParser XML = xmlParser
--withParser DOC = docParser
--withParser ODS = odsParser
withParser _   = error "[ERROR] Parser not implemented yet"


runParser :: FileFormat -> ByteString -> Either ErrorMessage (IO (Maybe Corpus))
runParser format text = parseOnly (withParser format) text


parseZip :: FilePath -> ByteString -> IO Corpus
parseZip = undefined

parseFile :: FileFormat -> ByteString -> IO Corpus
parseFile p x = case runParser p x of
        Left  _ -> pure 0
        Right r -> pure $ length r


openZipFiles :: FilePath -> IO [ByteString]
openZipFiles fp = do
    path    <- resolveFile' fp
    entries <- withArchive path (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive path (getEntry s)) entries
    pure bs


wosParserTest :: FilePath -> IO [Int]
wosParserTest fp = join $ mapConcurrently (parseFile WOS) <$> openZipFiles fp


