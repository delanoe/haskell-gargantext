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
import qualified Data.Set        as S
import qualified Data.List       as L
import Data.Tuple.Extra (both)
----------------------------------------------
import Gargantext.Core (Lang(FR))
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (score, createIndices, toIndex, fromIndex, cooc2mat, mat2map)
import Gargantext.Viz.Graph.Distances.Matrice (conditional', conditional)
import Gargantext.Viz.Graph.Index (Index)
import Gargantext.Text.Metrics.Count (cooc, removeApax)
import Gargantext.Text.Terms (TermType(Multi, Mono), extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)

--filterCooc :: Ord t => Map (t, t) Int -> Map (t, t) Int
--filterCooc m = 
---- filterCooc m = foldl (\k -> maybe (panic "no key") identity $ M.lookup k m) M.empty selection
----(ti, fi)  = createIndices m
-- . fromIndex fi $ filterMat $ cooc2mat ti m



import Data.Array.Accelerate (Matrix)
filterMat :: Matrix Int -> [(Index, Index)]
filterMat m = S.toList $ S.take n $ S.fromList $ (L.take nIe incExc') <> (L.take nSg speGen')
  where
    (incExc', speGen') = both ( map fst . L.sortOn snd . M.toList . mat2map) (conditional' m)
    n = nIe + nSg
    nIe = 30
    nSg = 70


pipeline path = do
  -- Text  <- IO Text <- FilePath
  text     <- readFile path
  let contexts = splitBy (Sentences 3) text
  myterms <- extractTerms Multi FR contexts
  
  -- TODO    filter (\t -> not . elem t stopList) myterms
  -- TODO    groupBy (Stem | GroupList)
  
  let myCooc = removeApax $ cooc myterms
  let (ti, fi) = createIndices myCooc
  pure ti
  -- Cooc -> Matrix
  
--  -- filter by spec/gen (dynmaic programming)
--  let theScores = M.filter (>0) $ score conditional myCoocFiltered
----
------  -- Matrix -> Clustering
------  pure $ bestpartition False $ map2graph $ toIndex ti theScores
--  partitions <- cLouvain theScores
--  pure partitions
---- | Building : -> Graph -> JSON


