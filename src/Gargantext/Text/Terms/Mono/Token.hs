{-|
Module      : Gargantext.Text.Ngrams.Token
Description : Tokens and tokenizing a text
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

In computer science, lexical analysis, lexing or tokenization is the
process of converting a sequence of characters (such as in a computer
program or web page) into a sequence of tokens (strings with an assigned
and thus identified meaning).
Source: https://en.wikipedia.org/wiki/Tokenize

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Mono.Token (tokenize)
  where

import Data.Text (Text)
import qualified Gargantext.Text.Terms.Mono.Token.En as En

import Gargantext.Core (Lang(..))
import Gargantext.Prelude

type Token = Text

-- >>> tokenize "A rose is a rose is a rose."
-- ["A","rose","is","a","rose","is","a","rose", "."]
-- 

data Context = Letter | Word | Sentence | Line | Paragraph

tokenize :: Text -> [Token]
tokenize = En.tokenize

tokenize' :: Lang -> Context -> [Token]
tokenize' = undefined

