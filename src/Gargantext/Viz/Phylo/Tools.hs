{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Phylo Toolbox:
- functions to build a Phylo
- functions to filter the cliques
- functions to manage a Phylo

Group Functions (TODO list)
- cohesion sur un groupe
- distance au dernier branchement
- âge du groupe

Futre Idea: temporal zoom on Phylo
phyloZoomOut :: (PeriodGrain, Phylo) -> [(PeriodGrain, Phylo)]
(from smallest granularity, it increases (zoom out) the periods of the Phylo)
Moral idea: viz from out to in

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.Tools where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Example

type MinSize = Int

-- | Building a phylo
-- (Indicative and schematic function)
-- buildPhylo :: Support -> MinSize
--                       -> Map Clique Support -> Phylo
-- buildPhylo s m mcs = level2Phylo
--                    . groups2level
--                    . clusters2group
--                    . Map.map clique2cluster
--                    . filterCliques s m

level2Phylo :: PhyloLevel -> Phylo -> Phylo
level2Phylo = undefined

groups2level :: [PhyloGroup] -> PhyloLevel
groups2level = undefined

-- clusters2group :: [Cluster Ngrams] -> PhyloGroup
-- clusters2group = undefined

-- clique2cluster :: Clique -> Cluster Ngrams
-- clique2cluster = undefined

-- | Filtering the cliques before bulding the Phylo
-- (Support and MinSize as parameter of the finale function to build a phylo)
-- idea: log of Corpus size (of docs)
filterCliques :: Support -> MinSize
                         -> Map Clique Support -> [Clique]
filterCliques s ms = maximalCliques
                   . filterWithSizeSet ms
                   . Map.keys
                   . filterWithSupport s

-- | Hapaxify / Threshold
-- hapax s = 1
-- ?  
filterWithSupport :: Support -> Map Clique Support -> Map Clique Support
filterWithSupport s = Map.filter (>s)

filterWithSizeSet :: MinSize -> [Clique] -> [Clique]
filterWithSizeSet = undefined

-- | filtre les cliques de ngrams compris dans une clique plus grande
-- /!\ optim inside
maximalCliques :: [Clique] -> [Clique]
maximalCliques = undefined


-- | Phylo management

-- | PhyloLevel Management
viewGroups :: (Start,End) -> PhyloLevel -> Phylo -> [PhyloGroup]
viewGroups = undefined

viewLevels :: (Start,End) -> Phylo -> [PhyloLevel]
viewLevels = undefined

-- | tous les terme des champs, tous les parents et les enfants
setGroup :: PhyloGroup -> PhyloGroup -> PhyloGroup
setGroup = undefined
--removeTerms :: recalculer les cliques pour ces termes
--addTerms 

