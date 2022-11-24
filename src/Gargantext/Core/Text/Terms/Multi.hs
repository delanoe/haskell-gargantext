{-|
Module      : Gargantext.Core.Text.Terms.Multi
Description : Multi Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Multi-terms are ngrams where n > 1.

-}


module Gargantext.Core.Text.Terms.Multi (multiterms, multiterms_rake, tokenTagsWith, tokenTags)
  where

import Data.Text hiding (map, group, filter, concat)
import Data.List (concat)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types
import Gargantext.Core.Utils (groupWithCounts)

import Gargantext.Core.Text.Terms.Multi.PosTagging
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import qualified Gargantext.Core.Text.Terms.Multi.Lang.En as En
import qualified Gargantext.Core.Text.Terms.Multi.Lang.Fr as Fr

import Gargantext.Core.Text.Terms.Multi.RAKE (multiterms_rake)
-- import qualified Gargantext.Utils.JohnSnowNLP as JohnSnow

import qualified Gargantext.Utils.SpacyNLP as SpacyNLP


-------------------------------------------------------------------
type NLP_API = Lang -> Text -> IO PosSentences

-------------------------------------------------------------------
multiterms :: Lang -> Text -> IO [TermsWithCount]
multiterms l txt = do
  ret <- multiterms' tokenTag2terms l txt
  pure $ groupWithCounts ret
  where
    multiterms' :: (TokenTag -> a) -> Lang -> Text -> IO [a]
    multiterms' f lang txt' = concat
                       <$> map (map f)
                       <$> map (filter (\t -> _my_token_pos t == Just NP))
                       <$> tokenTags lang txt'

-------------------------------------------------------------------
tokenTag2terms :: TokenTag -> Terms
tokenTag2terms (TokenTag ws t _ _) =  Terms ws t

tokenTags :: Lang -> Text -> IO [[TokenTag]]
tokenTags EN txt = tokenTagsWith EN txt corenlp
tokenTags FR txt = tokenTagsWith FR txt SpacyNLP.nlp
tokenTags l  _   = panic $ "[G.C.T.T.Multi] Lang NLP API not implemented yet " <> (cs $ show l)

tokenTagsWith :: Lang -> Text -> NLP_API -> IO [[TokenTag]]
tokenTagsWith lang txt nlp = map (groupTokens lang)
                         <$> map tokens2tokensTags
                         <$> map _sentenceTokens
                         <$> _sentences
                         <$> nlp lang txt


---- | This function analyses and groups (or not) ngrams according to
----   specific grammars of each language.
groupTokens :: Lang -> [TokenTag] -> [TokenTag]
groupTokens EN = En.groupTokens
groupTokens FR = Fr.groupTokens
groupTokens _  = panic $ pack "groupTokens :: Lang not implemeted yet"
