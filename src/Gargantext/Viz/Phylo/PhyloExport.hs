{-|
Module      : Gargantext.Viz.Phylo.PhyloExport
Description : Exportation module of a Phylo
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

module Gargantext.Viz.Phylo.PhyloExport where

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools 

import Data.GraphViz.Types.Generalised (DotGraph)

--------------------
-- | Dot export | --
--------------------


toDot :: [PhyloGroup] -> DotGraph DotId
toDot branches = undefined


----------------------
-- | post process | --
----------------------


processFilters :: [PhyloGroup] -> [PhyloGroup]
processFilters branches = branches

processSort :: [PhyloGroup] -> [PhyloGroup]
processSort branches = branches

processMetrics :: [PhyloGroup] -> [PhyloGroup]
processMetrics branches = branches

processDynamics :: [PhyloGroup] -> [PhyloGroup]
processDynamics branches = branches

processLabels :: [PhyloGroup] -> [PhyloGroup]
processLabels branches = branches

phyloPostProcess :: [PhyloGroup] -> [PhyloGroup]
phyloPostProcess branches = branches


---------------------
-- | phyloExport | --
---------------------   


toPhyloExport :: Phylo -> DotGraph DotId
toPhyloExport phylo = toDot
                    $ phyloPostProcess groups
    where 
        groups :: [PhyloGroup]
        groups =  getGroupsFromLevel (phyloLevel $ getConfig phylo) phylo