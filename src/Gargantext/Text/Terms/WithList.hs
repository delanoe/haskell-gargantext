{-|
Module      : Gargantext.Text.Terms.WithList
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns      #-}

module Gargantext.Text.Terms.WithList where

import qualified Data.Algorithms.KMP as KMP
import Data.Text (Text)
import qualified Data.IntMap.Strict as IntMap

import Gargantext.Core.Types (Terms(Terms))
import Gargantext.Text.Context
import Gargantext.Text.Terms.Mono (monoTextsBySentence)

import Gargantext.Prelude
import Data.List (concatMap)
import Data.Ord
import qualified Data.Set as Set


------------------------------------------------------------------------

data Pattern = Pattern
  { _pat_table  :: !(KMP.Table Term)
  , _pat_length :: !Int
  , _pat_terms  :: !Terms
  }
type Patterns = [Pattern]

------------------------------------------------------------------------

replaceTerms :: Patterns -> Sentence Term -> Sentence Terms
replaceTerms pats terms = go 0
  where
    terms_len = length terms

    go ix | ix >= terms_len = []
          | otherwise =
      case IntMap.lookup ix m of
        Nothing -> go (ix + 1)
        Just (len, terms) ->
          terms : go (ix + len)


    merge (len1, lab1) (len2, lab2) =
      if len2 < len1 then (len1, lab1) else (len2, lab2)

    m =
      IntMap.fromListWith merge
        [ (ix, (len, terms))
        | Pattern pat len terms <- pats, ix <- KMP.match pat terms ]

buildPatterns :: TermList -> Patterns
buildPatterns = sortWith (Down . _pat_length) . concatMap buildPattern
  where
    buildPattern (label, alts) = map f (label : alts)
      where
        f alt = Pattern (KMP.build alt) (length alt)
                        (Terms label $ Set.empty) -- TODO check stems

extractTermsWithList :: Patterns -> Text -> Corpus Terms
extractTermsWithList pats = map (replaceTerms pats) . monoTextsBySentence
