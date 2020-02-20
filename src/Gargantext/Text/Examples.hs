{-|
Module      : Gargantext.Text.Examples
Description : Minimal Examples to test behavior of the functions.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This file is intended for these purposes:

- documentation for teaching and research
- learn basics of Haskell which is a scientific programming language
- behavioral tests (that should be completed with uni-tests and scale-tests)

This document defines basic of Text definitions according to Gargantext..

- What is a term ?
- What is a sentence ?
- What is a paragraph ?

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Examples
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

-- | Sentences
-- Let be a list of Texts: ['Data.Text.Text']. Each text in this example is a sentence.
--
-- >>> ex_sentences
-- ["There is a table with a glass of wine and a spoon.","I can see the glass on the table.","There was only a spoon on that table.","The glass just fall from the table, pouring wine everywhere.","I wish the glass did not contain wine."]
ex_sentences :: [Text]
ex_sentences = [ "There is a table with a glass of wine and a spoon."
            , "I can see the glass on the table."
            , "There was only a spoon on that table."
            , "The glass just fall from the table, pouring wine everywhere."
            , "I wish the glass did not contain wine."
            ]


-- | From list to simple text as paragraph.
-- Let 'Data.Text.intercalate' each sentence with a space. Result is a paragraph.
--
-- >>> T.intercalate (T.pack " ") ex_sentences
-- "There is a table with a glass of wine and a spoon. I can see the glass on the table. There was only a spoon on that table. The glass just fall from the table, pouring wine everywhere. I wish the glass did not contain wine."
ex_paragraph :: Text
ex_paragraph = T.intercalate " " ex_sentences

-- | Let split sentences by Contexts of text.
-- More about 'Gargantext.Text.Context'
--
-- >>> ex_sentences == splitBy (Sentences 0) ex_paragraph
-- True

-- | Terms reordered to visually check occurrences
-- Split text by sentence and then extract ngrams.
--
-- >>> mapM (terms (MonoMulti EN)) $ splitBy (Sentences 0) ex_paragraph
-- [[["table"],["glass"],["wine"],["spoon"]],[["glass"],["table"]],[["spoon"],["table"]],[["glass"],["table"],["wine"]],[["glass"],["wine"]]]
ex_terms :: IO [[Terms]]
ex_terms = mapM (terms (MonoMulti EN)) $ splitBy (Sentences 0) ex_paragraph

-- | Test the Occurrences
--
-- >>> occurrences <$> L.concat <$> ex_terms
-- fromList [(fromList ["glass"],fromList [(["glass"],4)]),(fromList ["spoon"],fromList [(["spoon"],2)]),(fromList ["tabl"],fromList [(["table"],4)]),(fromList ["wine"],fromList [(["wine"],3)])]
ex_occ :: IO (Map Grouped (Map Terms Int))
ex_occ = occurrences <$> L.concat <$> ex_terms

-- | Test the cooccurrences
-- Use the 'Gargantext.Text.Metrics.Count.cooc' function.
--
-- >>> cooc <$> ex_terms
-- fromList [((["glass"],["glass"]),4),((["spoon"],["glass"]),1),((["spoon"],["spoon"]),2),((["table"],["glass"]),3),((["table"],["spoon"]),2),((["table"],["table"]),4),((["wine"],["glass"]),3),((["wine"],["spoon"]),1),((["wine"],["table"]),2),((["wine"],["wine"]),3)]
ex_cooc :: IO (Map (Label, Label) Int)
ex_cooc = cooc <$> ex_terms

-- | Tests the specificity and genericity
--
-- >>> ex_cooc_mat
-- (fromList [(["glass"],0),(["spoon"],1),(["table"],2),(["wine"],3)],Matrix (Z :. 4 :. 4) 
--   [ 4, 0, 0, 0,
--     1, 2, 0, 0,
--     3, 2, 4, 0,
--     3, 1, 2, 3],Matrix (Z :. 4 :. 4) 
--   [ 1.0, 0.25, 0.75, 0.75,
--     0.0,  1.0,  1.0,  0.5,
--     0.0,  0.0,  1.0,  0.5,
--     0.0,  0.0,  0.0,  1.0],(Vector (Z :. 4) [0.5833333333333334,0.5833333333333334,0.75,0.5833333333333334],Vector (Z :. 4) [-0.5833333333333334,-0.4166666666666667,0.41666666666666674,0.5833333333333334]))
ex_cooc_mat :: IO (Map Label Index, Matrix Int, Matrix Double, (DAA.Vector InclusionExclusion, DAA.Vector SpecificityGenericity))
ex_cooc_mat = do
  m <- ex_cooc
  let (ti,_) = createIndices m
  let mat_cooc = cooc2mat ti m
  pure ( ti
       , mat_cooc
       , incExcSpeGen_proba  mat_cooc
       , incExcSpeGen        mat_cooc
       )

ex_incExcSpeGen :: IO ([(Label, Double)], [(Label, Double)])
ex_incExcSpeGen = incExcSpeGen_sorted <$> ex_cooc

incExcSpeGen_sorted :: Ord t => Map (t,t) Int -> ([(t,Double)],[(t,Double)])
incExcSpeGen_sorted m = both ordonne (incExcSpeGen $ cooc2mat ti m)
  where
    (ti,fi) = createIndices m
    ordonne x = sortWith (Down . snd)
              $ zip (map snd $ M.toList fi) (toList x)


