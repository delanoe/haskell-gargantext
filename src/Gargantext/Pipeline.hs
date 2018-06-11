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
import Gargantext.Viz.Graph (Graph)
import Gargantext.Text.Metrics.Count (cooc, removeApax)
import Gargantext.Text.Metrics
import Gargantext.Text.Terms (TermType(Multi, Mono), extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain, LouvainNode)


{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/

-}

-----------------------------------------------------------
data2graph :: Map (Int, Int) Int -> Map (Int, Int) Double -> [LouvainNode] -> Graph
data2graph = undefined
-----------------------------------------------------------


workflow lang path = do
  -- Text  <- IO Text <- FilePath
  text     <- readFile path
  let contexts = splitBy (Sentences 5) text
  myterms <- extractTerms Multi lang contexts
  
  -- TODO    filter (\t -> not . elem t stopList) myterms
  -- TODO    groupBy (Stem | GroupList)
  
  -- @np FIXME optimization issue of filterCooc (too much memory consumed)
  let myCooc = filterCooc $ removeApax $ cooc myterms
  --pure myCooc
  -- Cooc -> Matrix
  let (ti, _) = createIndices myCooc
  -- Matrix -> Clustering
  let distance   = score conditional $ toIndex ti myCooc
  partitions <- cLouvain distance
  --pure partitions
---- | Building : -> Graph -> JSON
  pure partitions
  --pure $ data2graph myCooc distance partitions

