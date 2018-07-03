{-|
Module      : Gargantext.Text.Context
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Context of text management tool, here are logic of main types.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Context
  where

import Data.Text (Text, pack, unpack)
import Data.String (IsString)

import Text.HTML.TagSoup (parseTags, isTagText, Tag(..))
import Gargantext.Text
import Gargantext.Prelude hiding (length)


------------------------------------------------------------------------

type Term = Text
type Label = Term

type Sentence  a = [a] -- or a nominal group
type Corpus    a = [Sentence a] -- a list of sentences

-- type ConText a = [Sentence a]
-- type Corpus a = [ConText a]


------------------------------------------------------------------------

data SplitContext = Chars Int | Sentences Int | Paragraphs Int

tag :: Text -> [Tag Text]
tag = parseTags

-- | splitBy contexts of Chars or Sentences or Paragraphs
-- >> splitBy (Chars 0) "abcde"
-- ["a","b","c","d","e"]
-- >> splitBy (Chars 1) "abcde"
-- ["ab","bc","cd","de"]
-- >> splitBy (Chars 2) "abcde"
-- ["abc","bcd","cde"]
splitBy :: SplitContext -> Text -> [Text]
splitBy (Chars     n)  = map pack        . chunkAlong (n+1) 1 . unpack
splitBy (Sentences n)  = map unsentences . chunkAlong (n+1) 1 . sentences
splitBy (Paragraphs _) = map unTag       . filter isTagText   . tag
  where
    unTag :: IsString p => Tag p -> p
    unTag (TagText x) = x
    unTag _           = ""


