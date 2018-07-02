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
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.IntMap.Strict as IntMap
import Gargantext.Prelude
import Data.List (concatMap)

type Term = Text

type Label = Term

type Pattern = KMP.Table Term

type TermList = [(Label, [[Term]])]

type Patterns = [(Pattern, Int, Label)]

isMultiTermSep :: Char -> Bool
isMultiTermSep = (`elem` ",.:;?!(){}[]")

type Sentence  a = [a] -- or a nominal group
type Corpus    a = [Sentence a] -- a list of sentences

replaceTerms :: Patterns -> Sentence Term -> Sentence Label
replaceTerms pats terms = go 0 terms
  where
    go _ [] = []
    go !ix (t:ts) =
      case IntMap.lookup ix m of
        Nothing -> t : go (ix + 1) ts
        Just (len, label) ->
          label : go (ix + len) (drop (len - 1) ts)

    -- TODO is it what we want?
    merge (len1, lab1) (len2, lab2) =
      if len1 > len2 then (len1, lab1) else (len2, lab2)

    m =
      IntMap.fromListWith merge
        [ (ix, (len, label))
        | (pat, len, label) <- pats, ix <- KMP.match pat terms ]

buildPatterns :: TermList -> Patterns
buildPatterns = concatMap buildPattern
  where
    buildPattern (label, alts) = map f alts
      where
        f alt = (KMP.build alt, length alt, label)

-- monoterms'' :: Lang -> Text -> [Terms]
-- monoterms'' l txt = map (text2terms l) $ monoterms txt

extractTermsWithList :: Patterns -> Text -> Corpus Label
extractTermsWithList pats =
  map (replaceTerms pats) .
  map (T.split isSpace) . -- text2terms
  T.split isMultiTermSep . T.toLower -- as in monoterms with a different list of seps
