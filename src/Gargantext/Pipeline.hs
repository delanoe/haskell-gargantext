{-|
Module      : Gargantext.Pipeline
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}

module Gargantext.Pipeline
  where

import Data.Text.IO (readFile)
import qualified Data.Map.Strict as M
----------------------------------------------
import Gargantext.Core (Lang(FR))
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (score, createIndices, toIndex)
import Gargantext.Viz.Graph.Distances.Matrice (distributional)
import Gargantext.Text.Metrics.Occurrences (cooc, removeApax)
import Gargantext.Text.Terms (TermType(Multi, Mono), extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Data.Graph.Clustering.Louvain (bestpartition)
import Data.Graph.Clustering.Louvain.Utils (map2graph)

pipeline path = do
  -- Text  <- IO Text <- FilePath
  text     <- readFile path  
  let contexts = splitBy (Sentences 3) text
  myterms <- extractTerms Multi FR contexts
  
  -- TODO    filter (\t -> not . elem t stopList) myterms
  -- TODO    groupBy (Stem | GroupList)
  
  let myCooc = removeApax $ cooc myterms
  
  -- Cooc -> Matrix
  let theScores = M.filter (/=0) $ score distributional myCooc
  let (ti, _) = createIndices theScores
  
  -- Matrix -> Clustering -> Graph -> JSON
  pure $ bestpartition False $ map2graph $ toIndex ti theScores

