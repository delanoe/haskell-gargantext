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

import Data.List        (notElem,last,head,union,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!),sortOn,sort,(\\))
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith,fromList,mapKeys,insert)
import Data.Maybe       (isNothing)
import Data.Set         (Set)
import Data.Text        (Text,unwords)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)

import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vector


-- | To add a new meta Metric to a PhyloBranch
addBranchMetrics :: PhyloBranchId -> Text -> Double -> PhyloView -> PhyloView
addBranchMetrics id lbl val v = over (phylo_viewBranches
                                     . traverse) 
                                    (\b -> if getBranchId b == id
                                           then b & phylo_branchMetrics %~ insert lbl [val] 
                                           else b) v


-- | To get the age (in year) of all the branches of a PhyloView
branchAge :: PhyloView -> PhyloView
branchAge v = foldl (\v' b -> let bId = (fst . head) b
                                  prds = sortOn fst $ map snd b
                              in addBranchMetrics bId "age" ((abs . fromIntegral) $ ((snd . last) prds) - ((fst . head) prds)) v') v
            $ groupBy ((==) `on` fst)
            $ sortOn fst
            $ map (\n -> (getNodeBranchId n, (fst . fst) $ getNodeId n))
            $ getNodesInBranches v


-- | To process a list of Metrics to a PhyloView
processMetrics :: [Metric] -> Phylo -> PhyloView -> PhyloView
processMetrics ms p v = foldl (\v' m -> case m of
                                        BranchAge -> branchAge v'
                                        _         -> panic "[ERR][Viz.Phylo.Example.processMetrics] metric not found") v ms


