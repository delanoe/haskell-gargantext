{-|
Module      : Gargantext.Ngrams.Metrics 
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Gargantext.Ngrams.Metrics (levenshtein
                                      , levenshteinNorm
                                      , damerauLevenshtein
                                      , damerauLevenshteinNorm
                                      , overlap
                                      , jaccard
                                      , hamming
                                      ) where

import Gargantext.Prelude

import Data.Text (Text)
import GHC.Real (Ratio)
import qualified Data.Text.Metrics as DTM

{- * Example de titre
-}

-- | This module provide metrics to compare Text
-- starting as an API rexporting main functions of the great lib
-- text-metrics of Mark Karpov

-- | Levenshtein Distance
-- In information theory, Linguistics and computer science, 
-- the Levenshtein distance is a string metric for measuring 
-- the difference between two sequences.
-- See: https://en.wikipedia.org/wiki/Levenshtein_distance
--
levenshtein :: Text -> Text -> Int
levenshtein = DTM.levenshtein

-- | Return normalized Levenshtein distance between two 'Text' values.
-- Result is a non-negative rational number (represented as @'Ratio'
-- 'Data.Numeric.Natural'@), where 0 signifies no similarity between the
-- strings, while 1 means exact match.
--
levenshteinNorm :: Text -> Text -> Ratio Int
levenshteinNorm = DTM.levenshteinNorm

-- | Return Damerau-Levenshtein distance between two 'Text' values. The 
-- function works like 'levenshtein', but the collection of allowed     
-- operations also includes transposition of two /adjacent/ characters. 
-- See also:                                                            
-- <https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance> 
--
damerauLevenshtein :: Text -> Text -> Int
damerauLevenshtein = DTM.damerauLevenshtein

-- damerau-Levenshtein distance normalized
--
damerauLevenshteinNorm :: Text -> Text -> Ratio Int
damerauLevenshteinNorm = DTM.damerauLevenshteinNorm

-- Treating inputs like sets

-- | Return overlap coefficient for two 'Text' values. Returned value   
-- is in the range from 0 (no similarity) to 1 (exact match). Return 1  
-- if both 'Text' values are empty.                                     
--
-- See also: <https://en.wikipedia.org/wiki/Overlap_coefficient>.
overlap :: Text -> Text -> Ratio Int
overlap = DTM.overlap


-- | Jaccard distance
-- measures dissimilarity between sample sets
jaccard :: Text -> Text -> Ratio Int
jaccard = DTM.jaccard

-- | Hamming Distance
-- In information theory, the Hamming distance between two strings of
-- equal length is the number of positions at which the corresponding
-- symbols are different. In other words, it measures the minimum number of
-- substitutions required to change one string into the other
-- See:  https://en.wikipedia.org/wiki/Hamming_distance

hamming :: Text -> Text -> Maybe Int
hamming = DTM.hamming


