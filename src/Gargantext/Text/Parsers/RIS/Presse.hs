{-|
Module      : Gargantext.Text.Parsers.RIS.Presse
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Presse RIS format parser en enricher.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Parsers.RIS.Presse (presseEnrich) where

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
import Gargantext.Text.Parsers.RIS (withField)
import qualified Data.List as DL
-------------------------------------------------------------

-------------------------------------------------------------
presseEnrich :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
presseEnrich = (withField "DA" presseDate)
             . (withField "LA" presseLang)
             . (map (first presseFields))

presseDate :: ByteString -> [(ByteString, ByteString)]
presseDate str = either (const []) identity $ parseOnly parseDate str

parseDate :: Parser [(ByteString, ByteString)]
parseDate = do
  day <- take 2 <* "/"
  mon <- take 2 <* "/"
  yea <- take 4
  pure $ map (first (\x -> "publication_" <> x))
       [ ("day",day)
       , ("month", mon)
       , ("year", yea)
       , ("date", yea <> "-" <> mon <> "-" <> day <> "T0:0:0")
       ]

presseLang :: ByteString -> [(ByteString, ByteString)]
presseLang "Français" = [("language", "FR")]
presseLang "English"  = [("language", "EN")]
presseLang _ = undefined


presseFields :: ByteString -> ByteString
presseFields champs
            | champs == "AU" = "authors"
            | champs == "TI" = "title"
            | champs == "JF" = "source"
            | champs == "DI" = "doi"
            | champs == "UR" = "url"
            | champs == "N2" = "abstract"
            | otherwise  = champs


{-
fixTitle :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
fixTitle ns = ns <> [ti, ab]
  where
    ti = case 
-}
