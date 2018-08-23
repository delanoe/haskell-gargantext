{-|
Module      : Gargantext.Text.Metrics.Examples
Description : Minimal Examples to test behavior of the functions.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This file is intended for these purposes:

- documentation for teaching and research
- behavioral tests (that should be completed with uni-tests and scale-tests

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics.Examples
  where

import Data.Ord (Down(..))
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map  as M

import Data.Text (Text)
import qualified Data.Text as T

import Data.Tuple.Extra (both)
import Data.Array.Accelerate (toList, Matrix)

import Gargantext.Prelude
import Gargantext.Text.Metrics.Count (occurrences, cooc)
import Gargantext.Text.Terms (TermType(MonoMulti), terms)
import Gargantext.Core (Lang(EN))
import Gargantext.Core.Types (Terms(..), Label)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))
import Gargantext.Text.Metrics.Count (Grouped)
import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index

import qualified Data.Array.Accelerate as DAA


-- | From list to simple text
-- 
-- >>> metrics_text
-- "There is a table with a glass of wine and a spoon. I can see the glass on the table. There was only a spoon on that table. The glass just fall from the table, pouring wine everywhere. I wish the glass did not contain wine."
metrics_text :: Text
metrics_text = T.intercalate " " metrics_sentences


-- | Sentences
--
-- >>> metrics_sentences
-- ["There is a table with a glass of wine and a spoon.","I can see the glass on the table.","There was only a spoon on that table.","The glass just fall from the table, pouring wine everywhere.","I wish the glass did not contain wine."]
metrics_sentences :: [Text]
metrics_sentences = [ "There is a table with a glass of wine and a spoon."
                    , "I can see the glass on the table."
                    , "There was only a spoon on that table."
                    , "The glass just fall from the table, pouring wine everywhere."
                    , "I wish the glass did not contain wine."
                    ]

metrics_sentences_Test :: Bool
metrics_sentences_Test = metrics_sentences == splitBy (Sentences 0) metrics_text

-- | Terms reordered to visually check occurrences
-- Split text by sentence and then extract ngrams.
--
-- >>> metrics_terms
-- [[["table"],["glass"],["wine"],["spoon"]],[["glass"],["table"]],[["spoon"],["table"]],[["glass"],["table"],["wine"]],[["glass"],["wine"]]]
metrics_terms :: IO [[Terms]]
metrics_terms = mapM (terms (MonoMulti EN)) $ splitBy (Sentences 0) metrics_text

-- | Test the Occurrences
--
-- >>> metrics_occ
-- fromList [(fromList ["glass"],fromList [(["glass"],4)]),(fromList ["spoon"],fromList [(["spoon"],2)]),(fromList ["tabl"],fromList [(["table"],4)]),(fromList ["wine"],fromList [(["wine"],3)])]
metrics_occ :: IO (Map Grouped (Map Terms Int))
metrics_occ = occurrences <$> L.concat <$> metrics_terms

-- | Test the cooccurrences
--
-- >>> metrics_cooc
-- fromList [((["glass"],["glass"]),4),((["spoon"],["glass"]),1),((["spoon"],["spoon"]),2),((["table"],["glass"]),3),((["table"],["spoon"]),2),((["table"],["table"]),4),((["wine"],["glass"]),3),((["wine"],["spoon"]),1),((["wine"],["table"]),2),((["wine"],["wine"]),3)]
metrics_cooc :: IO (Map (Label, Label) Int)
metrics_cooc = cooc <$> metrics_terms

-- | Tests 
metrics_cooc_mat :: IO (Map Label Index, Matrix Int, Matrix Double, (DAA.Vector InclusionExclusion, DAA.Vector SpecificityGenericity))
metrics_cooc_mat = do
  m <- metrics_cooc
  let (ti,_) = createIndices m
  let mat_cooc = cooc2mat ti m
  pure ( ti
       , mat_cooc
       , incExcSpeGen_proba  mat_cooc
       , incExcSpeGen        mat_cooc
       )

metrics_incExcSpeGen :: IO ([(Label, Double)], [(Label, Double)])
metrics_incExcSpeGen = incExcSpeGen_sorted <$> metrics_cooc

incExcSpeGen_sorted :: Ord t => Map (t,t) Int -> ([(t,Double)],[(t,Double)])
incExcSpeGen_sorted m = both ordonne (incExcSpeGen $ cooc2mat ti m)
  where
    (ti,fi) = createIndices m
    ordonne x = sortWith (Down . snd) $ zip (map snd $ M.toList fi) (toList x)


