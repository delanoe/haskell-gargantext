{-|
Module      : Gargantext.Text.Ngrams
Description : Ngrams definition and tools
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

An @n-gram@ is a contiguous sequence of n items from a given sample of
text. In Gargantext application the items are words, n is a non negative
integer.

Using Latin numerical prefixes, an n-gram of size 1 is referred to as a
"unigram"; size 2 is a "bigram" (or, less commonly, a "digram"); size
3 is a "trigram". English cardinal numbers are sometimes used, e.g.,
"four-gram", "five-gram", and so on.

Source: https://en.wikipedia.org/wiki/Ngrams

TODO
group Ngrams -> Tree
compute occ by node of Tree
group occs according groups

compute cooccurrences
compute graph

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms
  where

import Data.Text (Text)

import Gargantext.Prelude
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Text.Terms.Multi (multiterms)
import Gargantext.Text.Terms.Mono  (monoterms')

data TermType = Mono | Multi

-- remove Stop Words
-- map (filter (\t -> not . elem t)) $ 
------------------------------------------------------------------------
terms :: TermType -> Lang -> Text -> IO [Terms]
terms Mono  lang txt = pure $ monoterms' lang txt
terms Multi lang txt = multiterms lang txt
------------------------------------------------------------------------

