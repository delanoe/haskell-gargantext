{-|
Module      : Gargantext.Ngrams.Parser
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Ngrams.Parser where

import Gargantext.Prelude
import Gargantext.Ngrams.CoreNLP
import Data.Text hiding (map)

import Gargantext.Types.Main (Language(..))
import qualified Gargantext.Ngrams.Lang.En as En
import qualified Gargantext.Ngrams.Lang.Fr as Fr

type SNgrams       = (Text, Text, Text)

-- | Ngrams selection algorithms
-- A form is a list of characters seperated by one or more spaces in a sentence.
-- A word is a form.

-- type Form = [Char]
-- For performance reasons, Type Text is used, then:
-- type Form = Text


-- Let be a form and its associated forms in contexts of a sentence.
-- Forms and subfoorms can be representend as Tree whose top is the minimal form
-- as a monogram whos occurrences are 

-- ps : Common words function in Haskell do not take sentence into account


-- TODO for scientific papers: add maesures
-- TODO add the p score regex
extractNgrams :: Language -> Text -> IO [[SNgrams]]
extractNgrams lang s = map (groupNgrams lang) <$> extractNgrams' lang s


extractNgrams' :: Language -> Text -> IO [[SNgrams]]
extractNgrams' lang t =  map (map token2text)
                     <$> map _sentenceTokens
                     <$> _sentences
                     <$> corenlp lang t

-- | This function selects ngrams according to grammars specific
--   of each language.
--   In english, JJ is ADJectiv in french.
selectNgrams :: Language -> [SNgrams] -> [SNgrams]
selectNgrams EN = En.selectNgrams
selectNgrams FR = Fr.selectNgrams

-- | This function analyze and groups (or not) ngrams according to 
--   grammars specific of each language.
groupNgrams :: Language -> [SNgrams] -> [SNgrams]
groupNgrams EN = En.groupNgrams
groupNgrams FR = Fr.groupNgrams

