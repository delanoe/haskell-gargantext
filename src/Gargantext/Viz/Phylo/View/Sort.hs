{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Viz.Phylo.View.Sort
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (sortOn)
import Data.Tuple       (fst, snd)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools


-- | To sort a PhyloView by Age
sortBranchByAge :: Order -> PhyloView -> PhyloView
sortBranchByAge o v = v & pv_branches %~ f
  where
    --------------------------------------
    f :: [PhyloBranch] -> [PhyloBranch] 
    f xs = case o of 
           Asc  -> sortOn (getBranchMeta "age") xs
           Desc -> reverse $ sortOn (getBranchMeta "age") xs
    --------------------------------------

-- | To sort a PhyloView by Birth date of a branch
sortBranchByBirth :: Order -> PhyloView -> PhyloView
sortBranchByBirth o v = v & pv_branches %~ f
  where
    --------------------------------------
    f :: [PhyloBranch] -> [PhyloBranch] 
    f xs = case o of 
           Asc  -> sortOn (getBranchMeta "birth") xs
           Desc -> reverse $ sortOn (getBranchMeta "birth") xs
    --------------------------------------    

-- | To process a Sort to a PhyloView
processSort :: Maybe (Sort,Order) -> Phylo -> PhyloView -> PhyloView 
processSort s _p v = case s of
                    Nothing -> v
                    Just s'  -> case fst s' of
                               ByBranchAge   -> sortBranchByAge   (snd s') v
                               ByBranchBirth -> sortBranchByBirth (snd s') v
                               --_           -> panic "[ERR][Viz.Phylo.View.Sort.processSort] sort not found"
