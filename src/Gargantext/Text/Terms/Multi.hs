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

module Gargantext.Text.Terms.Multi (multiterms, multiterms_rake)
  where

import Data.Text hiding (map, group, filter, concat)
import Data.List (concat)
import qualified Data.Set as S

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types

import Gargantext.Text.Terms.Multi.PosTagging
import Gargantext.Text.Terms.Mono.Stem (stem)
import qualified Gargantext.Text.Terms.Multi.Lang.En as En
import qualified Gargantext.Text.Terms.Multi.Lang.Fr as Fr

import Gargantext.Text.Terms.Multi.RAKE (multiterms_rake)

multiterms :: Lang -> Text -> IO [Terms]
multiterms lang txt = concat
                   <$> map (map (tokenTag2terms lang))
                   <$> map (filter (\t -> _my_token_pos t == Just NP)) 
                   <$> tokenTags lang txt

tokenTag2terms :: Lang -> TokenTag -> Terms
tokenTag2terms lang (TokenTag w t _ _) =  Terms w t'
  where
    t' = S.fromList $ map (stem lang) $ S.toList t

tokenTags :: Lang -> Text -> IO [[TokenTag]]
tokenTags lang s = map (group lang) <$> tokenTags' lang s


tokenTags' :: Lang -> Text -> IO [[TokenTag]]
tokenTags' lang t =  map tokens2tokensTags
                     <$> map _sentenceTokens
                     <$> _sentences
                     <$> corenlp lang t

---- | This function analyses and groups (or not) ngrams according to
----   specific grammars of each language.
group :: Lang -> [TokenTag] -> [TokenTag]
group EN = En.group
group FR = Fr.group

