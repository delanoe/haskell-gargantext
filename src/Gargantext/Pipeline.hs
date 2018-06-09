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

import Control.Arrow ((***))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List       as L
import Data.Tuple.Extra (both)
----------------------------------------------
import Gargantext.Core (Lang(FR))
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (score, createIndices, toIndex, fromIndex, cooc2mat, mat2map)
import Gargantext.Viz.Graph.Distances.Matrice (conditional', conditional)
import Gargantext.Viz.Graph.Index (Index)
import Gargantext.Text.Metrics.Count (cooc, removeApax)
import Gargantext.Text.Metrics
import Gargantext.Text.Terms (TermType(Multi, Mono), extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)


{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/

-}

workflow lang path = do
  -- Text  <- IO Text <- FilePath
  text     <- readFile path
  let contexts = splitBy (Sentences 5) text
  myterms <- extractTerms Multi lang contexts
  
  -- TODO    filter (\t -> not . elem t stopList) myterms
  -- TODO    groupBy (Stem | GroupList)
  
  let myCooc = filterCooc $ removeApax $ cooc myterms
  -- Cooc -> Matrix
  --let (ti, fi) = createIndices myCooc
  -- @np FIXME optimization issue of filterCooc (too much memory consumed)
  pure myCooc
  -- Matrix -> Clustering
-- pure $ bestpartition False $ map2graph $ toIndex ti myCooc
  --partitions <- cLouvain $ toIndex ti $ M.map (\v -> (fromIntegral v) :: Double) myCooc
  --pure partitions
---- | Building : -> Graph -> JSON

