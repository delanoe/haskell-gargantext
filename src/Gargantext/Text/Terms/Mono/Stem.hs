{-|
Module      : Gargantext.Text.Ngrams.Stem
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

In linguistic morphology and information retrieval, stemming is the
process of reducing inflected (or sometimes derived) words to their word
stem, base or root form—generally a written word form. The @stem@ needs
not be identical to the morphological root of the word; it is usually
sufficient that related words map to the same stem, even if this stem is
not in itself a valid root.
Source : https://en.wikipedia.org/wiki/Stemming

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Mono.Stem (stem, Lang(..))
  where

import Data.Text (Text)
import qualified Data.Text   as DT
import qualified NLP.Stemmer as N

import Gargantext.Prelude
import Gargantext.Core (Lang(..))

-- (stem, Stemmer(..))

--import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
--import Language.Aspell.Options (ACOption(..))


-- | Stemmer

-- A stemmer for English, for example, should identify the string "cats"
-- (and possibly "catlike", "catty" etc.) as based on the root "cat".

-- and
-- "stems", "stemmer", "stemming", "stemmed" as based on "stem". A stemming
-- algorithm reduces the words "fishing", "fished", and "fisher" to the
-- root word, "fish". On the other hand, "argue", "argued", "argues",
-- "arguing", and "argus" reduce to the stem "argu" (illustrating the
-- case where the stem is not itself a word or root) but "argument" and
-- "arguments" reduce to the stem "argument".


stem :: Lang -> Text -> Text
stem lang = DT.pack . N.stem lang' . DT.unpack
  where
    lang' = case lang of
              EN -> N.English
              FR -> N.French
            --_  -> panic $ DT.pack "not implemented yet"



