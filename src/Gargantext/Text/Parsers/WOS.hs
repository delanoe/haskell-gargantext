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
notice = start *> fields <* end
    where
      start :: Parser ByteString
      start = "\nPT " *> takeTill isEndOfLine

      end :: Parser [Char]
      end = manyTill anyChar (string $ pack "\nER\n")


fields :: Parser [(ByteString, ByteString)]
fields = many field
    where
        field :: Parser (ByteString, ByteString)
        field = do
            name  <- "\n" *> take 2 <* " "
            txt   <- takeTill isEndOfLine
            txts  <- try lines
            let txts' = case DL.length txts > 0 of
                    True  -> txts
                    False -> []
            pure (translate name, concat ([txt] <> txts'))


lines :: Parser [ByteString]
lines = many line
    where
        line :: Parser ByteString
        line = "\n  " *> takeTill isEndOfLine

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

