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

module Gargantext.Viz.Phylo.View.Filters
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (notElem,null,nub,(\\),intersect)
import Data.Maybe       (isNothing)
import Data.Tuple       (fst)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools


-- | To clean a PhyloView list of Nodes, Edges, etc after having filtered its Branches
cleanNodesEdges :: PhyloView -> PhyloView -> PhyloView
cleanNodesEdges v v' = v' & pv_nodes %~ (filter (\n -> not $ elem (getNodeId n) nIds))
                          & pv_nodes %~ (map (\n -> if isNothing (n ^. pn_parents)
                                                           then n
                                                           else if (not .null) $ (getNodeParentsId n) `intersect` nIds
                                                                then n & pn_parents .~ Nothing
                                                                else n ))
                          & pv_edges %~ (filter (\e -> (not $ elem (getSourceId e) nIds)
                                                           && (not $ elem (getTargetId e) nIds)))
  where
    --------------------------------------
    nIds :: [PhyloGroupId]
    nIds = map getNodeId
         $ filter (\n -> elem (getNodeBranchId n) bIds)
         $ getNodesInBranches v
    --------------------------------------
    bIds :: [PhyloBranchId]
    bIds = (getViewBranchIds v) \\ (getViewBranchIds v')
    --------------------------------------


-- | To filter all the LonelyBranches (ie: isolated one in time & with a small number of nodes) of a PhyloView
filterLonelyBranch :: Int -> Int -> Int -> [PhyloPeriodId] -> PhyloView -> PhyloView
filterLonelyBranch inf sup min' prds v = cleanNodesEdges v v'
  where
    --------------------------------------
    v' :: PhyloView
    v' = v & pv_branches %~ (filter (\b -> let
                                              ns = filter (\n -> (getBranchId b)  == (getNodeBranchId n)) $ getNodesInBranches v
                                              prds' = nub $ map (\n -> (fst . fst) $ getNodeId n) ns
                                                  in not (isLone ns prds')))
    --------------------------------------
    isLone :: [PhyloNode] -> [PhyloPeriodId] -> Bool
    isLone ns prds' = (length ns <= min')
                      && notElem (head' "filterLonelyBranch1" prds') (take inf prds)
                      && notElem (head' "filterLonelyBranch2" prds') (take sup $ reverse prds)
    --------------------------------------

-- | To filter all the branches with a minimal size in a PhyloView
filterSizeBranch :: Int -> PhyloView -> PhyloView
filterSizeBranch min' v = cleanNodesEdges v v'
  where
    --------------------------------------
    v' :: PhyloView
    v' = v & pv_branches %~ (filter (\b -> (length $ filter (\n -> (getBranchId b)  == (getNodeBranchId n)) $ getNodesInBranches v) >= min'))
    --------------------------------------


-- | To process a list of QueryFilter to a PhyloView
processFilters :: [Filter] -> Phylo -> PhyloView -> PhyloView
processFilters fs p v = foldl (\v' f -> case f of
                                        LonelyBranch (LBParams inf sup min') -> filterLonelyBranch inf sup min' (getPhyloPeriods p) v'
                                        SizeBranch (SBParams min')            -> filterSizeBranch min' v'
                                      -- _   -> panic "[ERR][Viz.Phylo.View.Filters.processFilters] filter not found"
                                       ) v fs
