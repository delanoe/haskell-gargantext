{-|
Module      : Gargantext.Core.Text.Context
Description : How to manage contexts of texts ?
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Context of text management tool, here are logic of main types:

- Term
- Multi-term
- Label
- Sentence
- Corpus

How to split contexts is describes in this module.

-}


module Gargantext.Core.Text.Context
  where

import Data.Text (Text, pack, unpack)
import Data.String (IsString)

import Text.HTML.TagSoup (parseTags, isTagText, Tag(..))
import Gargantext.Core.Text
import Gargantext.Prelude hiding (length)

------------------------------------------------------------------------
type Term = Text
type MultiTerm = [Term]
type Label = MultiTerm

type TermList = [(Label, [MultiTerm])]

type Sentence  a = [a] -- or a nominal group
type Corpus    a = [Sentence a] -- a list of sentences

-- type ConText a = [Sentence a]
-- type Corpus a = [ConText a]
------------------------------------------------------------------------

-- | Contexts definition to build/unbuild contexts.
data SplitContext = Chars Int | Sentences Int | Paragraphs Int

-- | splitBy contexts of Chars or Sentences or Paragraphs
-- To see some examples at a higher level (sentences and paragraph), see
-- 'Gargantext.Core.Text.Examples.ex_terms'
--
-- >>> splitBy (Chars 0) (pack "abcde")
-- ["a","b","c","d","e"]
--
-- >>> splitBy (Chars 1) (pack "abcde")
-- ["ab","bc","cd","de"]
--
-- >>> splitBy (Chars 2) (pack "abcde")
-- ["abc","bcd","cde"]
splitBy :: SplitContext -> Text -> [Text]
splitBy (Chars     n)  = map pack        . chunkAlong (n+1) 1 . unpack
splitBy (Sentences n)  = map unsentences . chunkAlong (n+1) 1 . sentences
splitBy (Paragraphs _) = map unTag       . filter isTagText   . parseTags
  where
    unTag :: IsString p => Tag p -> p
    unTag (TagText x) = x
    unTag _           = ""

