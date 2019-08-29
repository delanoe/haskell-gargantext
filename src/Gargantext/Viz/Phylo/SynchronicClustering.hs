{-|
Module      : Gargantext.Viz.Phylo.SynchronicClustering
Description : Module dedicated to the adaptative synchronic clustering of a Phylo.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.Phylo.SynchronicClustering where

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools

import Data.List (foldl', (++), null, intersect, (\\), union, nub, concat)

--------------------
-- | Clustering | --
--------------------


relatedComponents :: Eq a => [[a]] -> [[a]]
relatedComponents graphs = foldl' (\mem groups -> 
  if (null mem)
  then mem ++ [groups]
  else 
    let related = filter (\groups' -> (not . null) $ intersect groups groups') mem
    in if (null related)
       then mem ++ [groups]
       else (mem \\ related) ++ [union groups (nub $ concat related)] ) [] graphs 