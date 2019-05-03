{-|
Module      : Gargantext.Text.Parsers.RIS
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


RIS is a standardized tag format developed by Research Information
Systems, Incorporated (the format name refers to the company) to enable
citation programs to exchange data.

[More](https://en.wikipedia.org/wiki/RIS_(file_format))

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Parsers.RIS (risParser, withField) where

import Data.Either (either)
import Data.List (lookup)
import Data.Tuple.Extra (first)
import Control.Applicative
import Data.Attoparsec.ByteString (Parser, try, string, takeTill, take, manyTill, many1, endOfInput, parseOnly)
import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
import Data.ByteString (ByteString, concat, length)
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Gargantext.Prelude hiding (takeWhile, take, concat, readFile, lines, concat)
import qualified Data.List as DL
-------------------------------------------------------------

risParser :: Parser [[(ByteString, ByteString)]]
risParser = do
    n  <- notice "TY  -"
    ns <- many1 (notice "\nTY  -")
    pure $ [n] <> ns

notice :: Parser ByteString -> Parser [(ByteString, ByteString)]
notice s = start *> many field <* end
    where
      start :: Parser ByteString
      start = s *> takeTill isEndOfLine

      end :: Parser ByteString
      end =  "\nER  -" *> takeTill isEndOfLine

field :: Parser (ByteString, ByteString)
field = do
    name  <- "\n" *> take 2 <* "  - "
    txt   <- takeTill isEndOfLine
    txts  <- try lines
    let txts' = case DL.length txts > 0 of
            True  -> txts
            False -> []
    pure (name, concat ([txt] <> txts'))

lines :: Parser [ByteString]
lines = many line
    where
        line :: Parser ByteString
        line = "\n\n" *> takeTill isEndOfLine

-------------------------------------------------------------
withField :: ByteString -> (ByteString -> [(ByteString, ByteString)])
       -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
withField k f m = m <> ( maybe [] f (lookup k m) )

