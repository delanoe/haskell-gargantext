{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.RIS
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


module Gargantext.Core.Text.Corpus.Parsers.RIS (parser, onField, fieldWith, lines) where

import Data.List (lookup)
import Control.Applicative
import Data.Attoparsec.ByteString (Parser, try, takeTill, take, many1)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.ByteString (ByteString, concat)
import Gargantext.Prelude hiding (takeWhile, take)
import qualified Data.List as DL
-------------------------------------------------------------

parser :: Parser [[(ByteString, ByteString)]]
parser = do
    n  <- notice "TY  -"
    ns <- many1 (notice "\nTY  -")
    pure $ [n] <> ns

notice :: Parser ByteString -> Parser [(ByteString, ByteString)]
notice s = start *> many (fieldWith field)  <* end
    where
      field :: Parser ByteString
      field = "\n" *> take 2 <* "  - "

      start :: Parser ByteString
      start = s *> takeTill isEndOfLine

      end :: Parser ByteString
      end =  "\nER  -" *> takeTill isEndOfLine


fieldWith :: Parser ByteString -> Parser (ByteString, ByteString)
fieldWith n = do
    name  <- n
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
        line = "\n " *> takeTill isEndOfLine

-------------------------------------------------------------
-- Field for First elem of a Tuple, Key for corresponding Map
onField :: ByteString -> (ByteString -> [(ByteString, ByteString)])
       -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
onField k f m = m <> ( maybe [] f (lookup k m) )



