{-|
Module      : Gargantext.Text.Terms.Multi
Description : Multi Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Multi-terms are ngrams where n > 1.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Multi (extractTokenTags)
  where

import Data.Text hiding (map, group)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types

import Gargantext.Text.Terms.Multi.PosTagging
import qualified Gargantext.Text.Terms.Multi.Lang.En as En
import qualified Gargantext.Text.Terms.Multi.Lang.Fr as Fr


extractTokenTags :: Lang -> Text -> IO [[TokenTag]]
extractTokenTags lang s = map (group lang) <$> extractTokenTags' lang s


extractTokenTags' :: Lang -> Text -> IO [[TokenTag]]
extractTokenTags' lang t =  map tokens2tokensTags
                     <$> map _sentenceTokens
                     <$> _sentences
                     <$> corenlp lang t

---- | This function analyses and groups (or not) ngrams according to
----   specific grammars of each language.
group :: Lang -> [TokenTag] -> [TokenTag]
group EN = En.group
group FR = Fr.group

