{-|
Module      : Gargantext.Text.Parsers.WOS
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

module Gargantext.Text.Parsers.WOS (wosParser) where

-- TOFIX : Should import Gargantext.Prelude here
import Prelude hiding (takeWhile, take, concat, readFile, lines, concat)

import qualified Data.List as DL

import Data.Monoid ((<>))
import Data.Attoparsec.ByteString (Parser, try, string
                                  , takeTill, take
                                  , manyTill, many1)
import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (pack)
import Control.Applicative
import Gargantext.Text.Parsers.RIS (fieldWith, lines)

-------------------------------------------------------------
-- | wosParser parses ISI format from
-- Web Of Science Database
wosParser :: Parser [[(ByteString, ByteString)]]
wosParser = do
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


translate :: ByteString -> ByteString
translate champs
            | champs == "AF" = "authors"
            | champs == "TI" = "title"
            | champs == "SO" = "source"
            | champs == "DI" = "doi"
            | champs == "PD" = "publication_date"
            | champs == "AB" = "abstract"
            | otherwise  = champs
-------------------------------------------------------------

