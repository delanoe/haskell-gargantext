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


module Gargantext.Core.Text.Corpus.Parsers.Iramuteq (parseIramuteqFile, parser, keys) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, takeTill, parseOnly)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, takeWhile, endOfLine)
import Data.ByteString (ByteString)
import Prelude hiding (takeWhile, take, concat, readFile, lines, concat)
import qualified Data.ByteString as DB

parseIramuteqFile :: FilePath -> IO (Either String [[(ByteString, ByteString)]])
parseIramuteqFile fp = do
  txts <- DB.readFile fp
  pure $ parseOnly parser txts

-------------------------------------------------------------
parser :: Parser [[(ByteString, ByteString)]]
parser = do
  ns <- (many notice)
  pure ns

notice :: Parser [(ByteString, ByteString)]
notice = do
    hs <- headers
    ns <- takeWhile (/= '*')
    pure $ hs <> [("text", ns)]

-----------------------------------------------------------------
headers :: Parser [(ByteString, ByteString)]
headers = parseOf header fields

header :: Parser ByteString
header = "**** " *> takeTill isEndOfLine <* endOfLine

-----------------------------------------------------------------
fields :: Parser [(ByteString, ByteString)]
fields = many (parseOf field fieldTuple)

field :: Parser ByteString
field =   "*" *> takeWhile (/= ' ') <* " "
      <|> "*" *> takeWhile (/= '\n')

fieldTuple :: Parser (ByteString, ByteString)
fieldTuple = do
  name <- takeWhile (/= '_') <* "_"
  rest <- takeWhile (/= '\n')
  pure (name,rest)

-----------------------------------------------------------------
constP :: Parser a -> ByteString -> Parser a
constP p t = case parseOnly p t of
  Left _ -> empty
  Right a -> return a

parseOf :: Parser ByteString -> Parser a -> Parser a
parseOf ptxt pa = bothParse <|> empty
  where
    bothParse = ptxt >>= constP pa

-----------------------------------------------------------------
-- These keys may not be constant for Iramuteq files formats
keys :: ByteString -> ByteString
keys f
      | f == "id"    = "doi"
      | f == "qui"   = "authors"
      | f == "quand" = "PY"
      | f == "type"  = "source"
      | f == "titre" = "title"
      | f == "ou"    = "institutes"
      | f == "text"  = "abstract"
      | otherwise  = f
