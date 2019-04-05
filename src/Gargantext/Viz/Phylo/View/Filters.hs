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
import Data.List        (notElem,head,null,nub,(\\),intersect)
import Data.Maybe       (isNothing)
import Data.Tuple       (fst)
import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools


-- | To clean a PhyloView list of Nodes, Edges, etc after having filtered its Branches
cleanNodesEdges :: PhyloView -> PhyloView -> PhyloView
cleanNodesEdges v v' = v' & phylo_viewNodes %~ (filter (\n -> not $ elem (getNodeId n) nIds))
                          & phylo_viewNodes %~ (map (\n -> if isNothing (n ^. phylo_nodeLevelParents)
                                                           then n
                                                           else if (not .null) $ (getNodeParentsId n) `intersect` nIds
                                                                then n & phylo_nodeLevelParents .~ Nothing
                                                                else n ))
                          & phylo_viewEdges %~ (filter (\e -> (not $ elem (getSourceId e) nIds)
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


-- | To filter all the SmallBranches (ie: isolated one in time & with a small number of nodes) of a PhyloView
filterSmallBranch :: Int -> Int -> Int -> [PhyloPeriodId] -> PhyloView -> PhyloView
filterSmallBranch inf sup min' prds v = cleanNodesEdges v v'
  where
    --------------------------------------
    v' :: PhyloView
    v' = v & phylo_viewBranches %~ (filter (\b -> let ns = filter (\n -> (getBranchId b)  == (getNodeBranchId n))
                                                         $ getNodesInBranches v
                                                      prds' = nub $ map (\n -> (fst . fst) $ getNodeId n) ns
                                                  in not (isLone ns prds')))
    --------------------------------------
    isLone :: [PhyloNode] -> [PhyloPeriodId] -> Bool
    isLone ns prds' = (length ns <= min')
                      && notElem (head prds') (take inf prds)
                      && notElem (head prds') (take sup $ reverse prds)
    --------------------------------------


-- | To process a list of QueryFilter to a PhyloView
processFilters :: [Filter] -> Phylo -> PhyloView -> PhyloView
processFilters fs p v = foldl (\v' f -> case f of
                                        SmallBranch (SBParams inf sup min') -> filterSmallBranch inf sup min'
                                                                               (getPhyloPeriods p) v'
                                      -- _   -> panic "[ERR][Viz.Phylo.View.Filters.processFilters] filter not found"
                                       ) v fs
