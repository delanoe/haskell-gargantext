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
import Data.Tuple.Extra (first)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Gargantext.Prelude hiding (takeWhile, take)
import Gargantext.Text.Parsers.RIS (withField)
import qualified Gargantext.Text.Parsers.Date.Attoparsec as Date
-------------------------------------------------------------
-------------------------------------------------------------
presseEnrich :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
presseEnrich = (withField "DA" presseDate)
             . (withField "LA" presseLang)
             . (map (first presseFields))

presseDate :: ByteString -> [(ByteString, ByteString)]
presseDate str = either (const []) identity $ parseOnly (Date.parserWith "/")  str

presseLang :: ByteString -> [(ByteString, ByteString)]
presseLang "Français" = [("language", "FR")]
presseLang "English"  = [("language", "EN")]
presseLang x = [("language", x)]

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
