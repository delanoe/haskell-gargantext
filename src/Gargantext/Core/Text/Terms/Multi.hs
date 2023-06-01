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

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Text.Terms.Multi (multiterms, multiterms_rake, tokenTagsWith, tokenTags, cleanTextForNLP)
  where

import Control.Applicative
import Data.Attoparsec.Text                               as DAT
import Data.List (concat)
import Data.Text hiding (map, group, filter, concat)
import Gargantext.Core (Lang(..), NLPServerConfig(..), PosTagAlgo(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Core.Text.Terms.Multi.RAKE (multiterms_rake)
import Gargantext.Core.Types
import Gargantext.Core.Utils (groupWithCounts)
import Gargantext.Prelude
import Replace.Attoparsec.Text                            as RAT
import qualified Gargantext.Core.Text.Terms.Multi.Lang.En as En
import qualified Gargantext.Core.Text.Terms.Multi.Lang.Fr as Fr
import qualified Gargantext.Utils.SpacyNLP                as SpacyNLP

-------------------------------------------------------------------
type NLP_API = Lang -> Text -> IO PosSentences

-------------------------------------------------------------------
multiterms :: NLPServerConfig -> Lang -> Text -> IO [TermsWithCount]
multiterms nsc l txt = do
  let txt' = cleanTextForNLP txt
  if txt' == ""
     then do
       printDebug "[G.C.T.Terms.Multi] becomes empty after cleanTextForNLP" txt
       pure []
     else do
       ret <- multiterms' tokenTag2terms l txt'
       pure $ groupWithCounts ret
    where
      multiterms' :: (TokenTag -> a) -> Lang -> Text -> IO [a]
      multiterms' f lang txt' = concat
                       <$> map (map f)
                       <$> map (filter (\t -> _my_token_pos t == Just NP))
                       <$> tokenTags nsc lang txt'

-------------------------------------------------------------------
tokenTag2terms :: TokenTag -> Terms
tokenTag2terms (TokenTag ws t _ _) =  Terms ws t

tokenTags :: NLPServerConfig -> Lang -> Text -> IO [[TokenTag]]
tokenTags (NLPServerConfig { server = CoreNLP, url }) EN txt = tokenTagsWith EN txt $ corenlp url
tokenTags (NLPServerConfig { server = Spacy, url }) l txt = do
  -- printDebug "NLP Debug" txt
  tokenTagsWith l txt $ SpacyNLP.nlp url
-- tokenTags FR txt = do
--   -- printDebug "[Spacy Debug]" txt
--   if txt == ""
--      then pure [[]]
--      else tokenTagsWith FR txt SpacyNLP.nlp
tokenTags _ l  _   = panic $ "[G.C.T.T.Multi] Lang NLP API not implemented yet " <> (cs $ show l)

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
groupTokens _  = Fr.groupTokens

-- TODO: make tests here
cleanTextForNLP :: Text -> Text
cleanTextForNLP = unifySpaces . removeDigitsWith "-" . removeUrls
  where
    remove x = RAT.streamEdit x (const "")

    unifySpaces         = RAT.streamEdit (many DAT.space) (const " ")
    removeDigitsWith x  = remove (many DAT.digit *> DAT.string x <* many DAT.digit)

    removeUrls          = removeUrlsWith "http" . removeUrlsWith "www"
    removeUrlsWith w    = remove (DAT.string w *> many (DAT.notChar ' ') <* many DAT.space)

