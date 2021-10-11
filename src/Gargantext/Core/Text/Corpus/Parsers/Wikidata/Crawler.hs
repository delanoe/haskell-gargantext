{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Wikidata.Crawler
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Thx to Alp Well Typed for the first version.

-}

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Text.Corpus.Parsers.Wikidata.Crawler
  where

import Control.Lens hiding (element, elements, children)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Gargantext.Prelude
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy.Lens



type WikipediaUrlPage = Text
crawlPage :: WikipediaUrlPage -> IO [Text]
crawlPage url = do
  datas <- get (unpack url)
  pure $ sectionsOf datas


sectionsOf :: Response ByteString -> [Text]
sectionsOf resp =
  resp ^.. responseBody
         . to (decodeUtf8With lenientDecode)
         . html
         . allAttributed (ix "class" . only "mw-parser-output")
         . allNamed (only "p")
         . to paragraphText

paragraphText :: Element -> Text
paragraphText p = collectTextN (p ^. children)
  where collectTextN (NodeContent t : ns) = t <> collectTextN ns
        collectTextN (NodeElement elt : ns) = collectTextE elt <> collectTextN ns
        collectTextN [] = ""

        collectTextE (Element _ _ ns) = collectTextN ns


