{-|
Module      : Gargantext.Text.Ngrams.Stem
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Gargantext.Text.Ngrams.Stem
  where

import Data.Text (Text)
import qualified Data.Text   as DT
import qualified NLP.Stemmer as N

import Gargantext.Core (Lang(..))

-- (stem, Stemmer(..))

--import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
--import Language.Aspell.Options (ACOption(..))

stem :: Lang -> Text -> Text
stem lang = DT.pack . N.stem lang' . DT.unpack
  where
    lang' = case lang of
              EN -> N.English
              FR -> N.French

