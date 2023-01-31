{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.WOS
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Gargantext.Core.Text.Corpus.Parsers.WOS (parser, keys) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, string, takeTill, take, manyTill, many1)
import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Gargantext.Core.Text.Corpus.Parsers.RIS (fieldWith)
import Prelude hiding (takeWhile, take, concat, readFile, lines, concat)

-------------------------------------------------------------
-- | wosParser parses ISI format from
-- Web Of Science Database
parser :: Parser [[(ByteString, ByteString)]]
parser = do
    -- TODO Warning if version /= 1.0
    -- FIXME anyChar (string ..) /= exact string "\nVR 1.0" ?
    _  <- manyTill anyChar (string $ pack "\nVR 1.0")
    ns <- many1 notice <*  (string $ pack "\nEF"    )
    pure ns

notice :: Parser [(ByteString, ByteString)]
notice = start *> many (fieldWith field) <* end
    where
      field :: Parser ByteString
      field = "\n" *> take 2 <* " "

      start :: Parser ByteString
      start = "\nPT " *> takeTill isEndOfLine

      end :: Parser [Char]
      end = manyTill anyChar (string $ pack "\nER\n")


keys :: ByteString -> ByteString
keys field
            | field == "AF" = "authors"
            | field == "TI" = "title"
            | field == "SO" = "source"
            | field == "DI" = "doi"
            | field == "PD" = "publication_date"
            | field == "SP" = "institutes"
            | field == "AB" = "abstract"
            | otherwise  = field
