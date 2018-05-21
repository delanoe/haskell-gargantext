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

------------------------------------------------------------------------
terms :: TermType -> Maybe Lang -> Text -> IO [Terms]
terms Mono  (Just lang)  txt = pure $ monoterms' lang txt
terms Multi (Just lang ) txt = multiterms lang txt
terms _      Nothing _ = panic "Lang needed"
------------------------------------------------------------------------

termTests :: Text
termTests = "It is hard to detect important articles in a specific context. Information retrieval techniques based on full text search can be inaccurate to identify main topics and they are not able to provide an indication about the importance of the article. Generating a citation network is a good way to find most popular articles but this approach is not context aware. The text around a citation mark is generally a good summary of the referred article. So citation context analysis presents an opportunity to use the wisdom of crowd for detecting important articles in a context sensitive way. In this work, we analyze citation contexts to rank articles properly for a given topic. The model proposed uses citation contexts in order to create a directed and edge-labeled citation network based on the target topic. Then we apply common ranking algorithms in order to find important articles in this newly created network. We showed that this method successfully detects a good subset of most prominent articles in a given topic. The biggest contribution of this approach is that we are able to identify important articles for a given search term even though these articles do not contain this search term. This technique can be used in other linked documents including web pages, legal documents, and patents as well as scientific papers."





