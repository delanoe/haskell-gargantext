{-|
Module      : Gargantext.Text.Context
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Context of text management tool

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Context where

import Data.Text (Text, pack, unpack, length)
import Data.String (IsString)

import Text.HTML.TagSoup
import Gargantext.Text
import Gargantext.Prelude hiding (length)

data SplitBy = Paragraph | Sentences | Chars


splitBy :: SplitBy -> Int -> Text -> [Text]
splitBy Chars     n = map pack . chunkAlong n n . unpack
splitBy Sentences n = map unsentences . chunkAlong n n  . sentences
splitBy Paragraph _ = map removeTag   . filter isTagText . parseTags
  where
    removeTag :: IsString p => Tag p -> p
    removeTag (TagText x) = x
    removeTag (TagComment x) = x
    removeTag _          = ""


