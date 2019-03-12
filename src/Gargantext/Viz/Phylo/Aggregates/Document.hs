{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.Aggregates.Document
  where

import Data.List        (last,head)
import Data.Map         (Map)
import Data.Text        (Text, unwords, toLower, words)
import Data.Tuple       (fst, snd)
import Data.Tuple.Extra
import Data.Vector      (Vector)

import Gargantext.Prelude                       hiding (head)
import Gargantext.Text.Terms.Mono               (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Vector as Vector   


-- | To init a set of periods out of a given Grain and Step 
docsToPeriods :: (Ord date, Enum date) => (doc -> date)
     -> Grain -> Step -> [doc] -> Map (date, date) [doc]
docsToPeriods _ _ _ [] = panic "[ERR][Viz.Phylo.Example.docsToPeriods] Empty [Documents] can not have any periods"
docsToPeriods f g s es = Map.fromList $ zip hs $ map (inPeriode f es) hs
  where
    --------------------------------------
    hs = steps g s $ both f (head es, last es)
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ List.partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------
    steps :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
    steps s' o' (start,end) = map (\l -> (head l, last l))
                          $ chunkAlong s' o' [start .. end]
    --------------------------------------


-- | To parse a list of Documents by filtering on a Vector of Ngrams 
parseDocs :: PhyloNgrams -> [Document] -> [Document]
parseDocs l docs = map (\(Document d t) 
                        -> Document d ( unwords 
                                      $ filter (\x -> Vector.elem x l)
                                      $ monoTexts t)) docs


-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: Grain -> Step -> [Document] -> PhyloNgrams -> Map (Date, Date) [Document]
groupDocsByPeriod g s docs ngrams = docsToPeriods date g s $ parseDocs ngrams docs 


-- | To transform a corpus of texts into a structured list of Documents
corpusToDocs :: [(Date, Text)] -> [Document]
corpusToDocs l = map (\(d,t) -> Document d t) l 