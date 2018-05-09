{-|
Module      : Gargantext.Text.Ngrams.PosTagging.Parser
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

Ngrams selection algorithms
A form is a list of characters seperated by one or more spaces in a sentence.
A word is a form.

-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Text.Ngrams.PosTagging.Parser
  where

import Data.Text hiding (map, group)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Text.Ngrams.PosTagging.CoreNLP

import qualified Gargantext.Text.Ngrams.PosTagging.Lang.En as En
import qualified Gargantext.Text.Ngrams.PosTagging.Lang.Fr as Fr


extractNgrams :: Lang -> Text -> IO [[NgramsTag]]
extractNgrams lang s = map (group lang) <$> extractNgrams' lang s


extractNgrams' :: Lang -> Text -> IO [[NgramsTag]]
extractNgrams' lang t =  map tokens2ngramsTags
                     <$> map _sentenceTokens
                     <$> _sentences
                     <$> corenlp lang t


---- | This function analyse and groups (or not) ngrams according to 
----   grammars specific of each language.
group :: Lang -> [NgramsTag] -> [NgramsTag]
group EN = En.group
group FR = Fr.group

