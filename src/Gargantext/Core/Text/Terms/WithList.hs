{-|
Module      : Gargantext.Core.Text.Terms.WithList
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}
{-# LANGUAGE BangPatterns      #-}

module Gargantext.Core.Text.Terms.WithList where

import Data.List (null)
import Data.Ord
import Data.Text (Text, concat, unwords)
import Gargantext.Prelude
import Gargantext.Core.Text.Context
import Gargantext.Core.Text.Terms.Mono (monoTextsBySentence)
import Gargantext.Core.Types (TermsCount)
import Gargantext.Core.Utils (groupWithCounts)
import Prelude (error)
import qualified Data.Algorithms.KMP as KMP
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.List           as List
------------------------------------------------------------------------

data Pattern = Pattern
  { _pat_table  :: !(KMP.Table Text)
  , _pat_length :: !Int
  , _pat_terms  :: ![Text]
  }
type Patterns = [Pattern]

------------------------------------------------------------------------

data ReplaceTerms = KeepAll | LongestOnly

replaceTerms :: ReplaceTerms -> Patterns -> [Text] -> [[Text]]
replaceTerms rplaceTerms pats terms = go 0
  where
    terms_len = length terms

    go ix | ix >= terms_len = []
          | otherwise =
      case IntMap.lookup ix m of
        Nothing -> go (ix + 1)
        Just (len, term) ->
          term : go (ix + len)

    m = toMap
        [ (ix, (len, term))
        | Pattern pat len term <- pats, ix <- KMP.match pat terms ]

    toMap = case rplaceTerms of
      KeepAll -> IntMap.fromList
      LongestOnly -> IntMap.fromListWith merge
        where
          merge (len1, lab1) (len2, lab2) =
            if len2 < len1 then (len1, lab1) else (len2, lab2)

buildPatterns :: TermList -> Patterns
buildPatterns = sortWith (Down . _pat_length) . concatMap buildPattern
  where
    buildPattern (label, alts) = map f $ map (\alt -> filter (/= "") alt) (label : alts)
      where
        f alt | "" `elem` alt = error ("buildPatterns: ERR1" <> show(label))
              | null alt      = error "buildPatterns: ERR2"
              | otherwise     =
                Pattern (KMP.build alt) (length alt) label
                        --(Terms label $ Set.empty) -- TODO check stems


--------------------------------------------------------------------------
-- Utils
type MatchedText = Text
termsInText :: Patterns -> Text -> [(MatchedText, TermsCount)]
termsInText pats txt = groupWithCounts
                     $ List.concat
                     $ map (map unwords)
                     $ extractTermsWithList pats txt

--------------------------------------------------------------------------

extractTermsWithList :: Patterns -> Text -> Corpus [Text]
extractTermsWithList pats = map (replaceTerms KeepAll pats) . monoTextsBySentence

-- | Extract terms
-- >>> let termList = [(["chat blanc"], [["chat","blanc"]])] :: TermList
-- extractTermsWithList' (buildPatterns termList) "Le chat blanc"["chat blanc"]
-- ["chat blanc"]
extractTermsWithList' :: Patterns -> Text -> [Text]
extractTermsWithList' pats = map (concat . map concat . replaceTerms KeepAll pats)
                           . monoTextsBySentence

--------------------------------------------------------------------------

{- | Not used
filterWith :: TermList
           -> (a -> Text)
           -> [a]
           -> [(a, [Text])]
filterWith termList f xs = filterWith' termList f zip xs


filterWith' :: TermList
           -> (a -> Text)
           -> ([a] -> [[Text]] -> [b])
           -> [a]
           -> [b]
filterWith' termList f f' xs = f' xs
                            $ map (extractTermsWithList' pats)
                            $ map f xs
    where
      pats = buildPatterns termList
-}
