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

module Gargantext.Viz.Phylo.Aggregates.Cooc
  where

import Data.List        (union,concat)
import Data.Map         (Map, elems, adjust)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.Map    as Map
import qualified Data.Set    as Set


-- | To transform the Fis into a coocurency Matrix in a Phylo
fisToCooc :: Map (Date, Date) [PhyloFis] -> Phylo -> Map (Int, Int) Double
fisToCooc m p = map   (/docs)
              $ foldl (\mem x -> adjust (+1) (getKeyPair x mem) mem) cooc
              $ concat
              $ map (\x -> listToUnDirectedCombiWith (\y -> getIdxInPeaks y p) $ (Set.toList . getClique) x)
              $ (concat . elems) m
  where
    --------------------------------------
    fisNgrams :: [Ngrams]
    fisNgrams = foldl (\mem x -> union mem $ (Set.toList . getClique) x) [] $ (concat . elems) m
    --------------------------------------
    docs :: Double
    docs = fromIntegral $ foldl (\mem x -> mem + (getSupport x)) 0 $ (concat . elems) m
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = Map.fromList $ map (\x -> (x,0)) (listToUnDirectedCombiWith (\y -> getIdxInPeaks y p) fisNgrams)
    --------------------------------------


-- phyloCooc :: Map (Int, Int) Double
-- phyloCooc = fisToCooc phyloFis phylo1_0_1
