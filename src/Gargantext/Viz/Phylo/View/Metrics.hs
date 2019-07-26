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

module Gargantext.Viz.Phylo.View.Metrics
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (last,groupBy,sortOn)
import Data.Map         (insert)
import Data.Text        (Text)
import Data.Tuple       (fst, snd)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools


-- | To add a new meta Metric to a PhyloBranch
addBranchMetrics :: PhyloBranchId -> Text -> Double -> PhyloView -> PhyloView
addBranchMetrics id lbl val v = over (pv_branches
                                     . traverse)
                                    (\b -> if getBranchId b == id
                                           then b & pb_metrics %~ insert lbl [val]
                                           else b) v


branchGroups :: PhyloView -> PhyloView
branchGroups v = foldl (\v' (bId,nb) -> addBranchMetrics bId "nbGroups" nb v') v
               $ map (\(bId,ns) -> (bId,fromIntegral $ length ns)) 
               $ getNodesByBranches v


-- | To get the age (in year) of all the branches of a PhyloView
branchAge :: PhyloView -> PhyloView
branchAge v = foldl (\v' b -> let bId = (fst . (head' "branchAge")) b
                                  prds = sortOn fst $ map snd b
                              in addBranchMetrics bId "age" ((abs . fromIntegral) $ ((snd . last) prds) - (fst $ head' "branchAge" prds)) v') v
            $ groupBy ((==) `on` fst)
            $ sortOn fst
            $ map (\n -> (getNodeBranchId n, (fst . fst) $ getNodeId n))
            $ getNodesInBranches v


-- | To get the age (in year) of all the branches of a PhyloView
branchBirth :: PhyloView -> PhyloView
branchBirth v = foldl (\v' b -> let bId = (fst . (head' "branchBirth")) b
                                    prds = sortOn fst $ map snd b
                              in addBranchMetrics bId "birth" (fromIntegral $ fst $ head' "branchAge" prds) v') v
            $ groupBy ((==) `on` fst)
            $ sortOn fst
            $ map (\n -> (getNodeBranchId n, (fst . fst) $ getNodeId n))
            $ getNodesInBranches v            


-- | To process a list of Metrics to a PhyloView
processMetrics :: [Metric] -> Phylo -> PhyloView -> PhyloView
processMetrics ms _p v = foldl (\v' m -> case m of
                                        BranchAge -> branchAge v'
                                        BranchBirth -> branchBirth v'
                                        BranchGroups -> branchGroups v'
                                       -- _         -> panic "[ERR][Viz.Phylo.Example.processMetrics] metric not found"
                                        ) v ms


