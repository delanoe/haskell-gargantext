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

module Gargantext.Viz.Phylo.BranchMaker
  where

import Control.Lens     hiding (both, Level)
import Data.List        (concat,nub,(++),tail,sortOn,take,reverse,sort,null,intersect,union)
import Data.Map         (Map)
import Data.Tuple       (fst, snd)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Metrics.Clustering
import Gargantext.Viz.Phylo.Aggregates.Cooc
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.LinkMaker

import qualified Data.Map as Map

-- import Debug.Trace (trace)

---------------------------
-- | Readability links | --
---------------------------

getGroupsPeriods :: [PhyloGroup] -> [(Date,Date)]
getGroupsPeriods gs = sortOn fst $ nub $ map getGroupPeriod gs

getFramedPeriod :: [PhyloGroup] -> (Date,Date)
getFramedPeriod gs = (fst $ (head' "getFramedPeriod" $ getGroupsPeriods gs), snd $ (last' "getFramedPeriod" $ getGroupsPeriods gs))


getGroupsNgrams :: [PhyloGroup] -> [Int]
getGroupsNgrams gs = (sort . nub . concat) $ map getGroupNgrams gs


-- | Get the Nth most coocurent Ngrams in a list of Groups
getGroupsPeaks :: [PhyloGroup] -> Int -> Phylo -> [Int]
getGroupsPeaks gs nth p = getNthMostOcc nth 
                        $ getSubCooc (getGroupsNgrams gs) 
                        $ getCooc (getGroupsPeriods gs) p


areDistant :: (Date,Date) -> (Date,Date) -> Int -> Bool
areDistant prd prd' thr = (((fst prd') - (snd prd)) > thr) || (((fst prd) - (snd prd')) > thr)


-- | Process a Jaccard on top of two set of Branch Peaks
areTwinPeaks :: Double -> [Int] -> [Int] -> Bool
areTwinPeaks thr ns ns' = ( ((fromIntegral . length) $ intersect ns ns') 
                          / ((fromIntegral . length) $ union ns ns')) >= thr 


findSimBranches :: Int -> Double -> Int -> Phylo -> (PhyloBranchId,[PhyloGroup]) -> [(PhyloBranchId,[PhyloGroup])] -> [(PhyloBranchId,[PhyloGroup])]
findSimBranches frame thr nth p (id,gs) bs 
  = filter (\(_  ,gs') -> areTwinPeaks thr pks (getGroupsPeaks gs' nth p))
  $ filter (\(_  ,gs') -> (not . null) $ intersect ns (getGroupsNgrams gs'))
  $ filter (\(_  ,gs') -> areDistant prd (getFramedPeriod gs') frame)
  $ filter (\(id',_  ) -> id /= id') bs
  where 
    --------------------------------------
    prd :: (Date,Date)
    prd = getFramedPeriod gs
    --------------------------------------
    ns :: [Int]
    ns = getGroupsNgrams gs
    --------------------------------------
    pks :: [Int]
    pks = getGroupsPeaks gs nth p
    --------------------------------------

findBestPointer :: Phylo -> Proximity -> [PhyloGroup] -> [PhyloGroup] -> [(PhyloGroupId,Pointer)]
findBestPointer p prox gs gs' = take 1
                              $ reverse 
                              $ sortOn (snd . snd) 
                              $ concat
                              $ map (\g -> let pts = findBestCandidates' prox gs' g p
                                           in map (\pt -> (getGroupId g,pt)) pts) gs

makeBranchLinks :: Phylo -> Proximity -> (PhyloBranchId,[PhyloGroup]) -> [(PhyloBranchId,[PhyloGroup])] -> [(PhyloGroupId,Pointer)] -> [(PhyloGroupId,Pointer)]
makeBranchLinks p prox (id,gs) bs pts
  | null bs   = pts
  | otherwise = makeBranchLinks p prox (head' "makeLink" bs) (tail bs) (pts ++ pts')
  where
    --------------------------------------
    pts' :: [(PhyloGroupId,Pointer)]
    pts' = concat $ map (\(_id,gs') -> findBestPointer p prox gs gs') candidates
    --------------------------------------
    candidates :: [(PhyloBranchId,[PhyloGroup])]
    candidates = findSimBranches (getPhyloMatchingFrame p) (getPhyloReBranchThr p) (getPhyloReBranchNth p) p (id,gs) bs

 

linkPhyloBranches :: Level -> Proximity -> Phylo -> Phylo
linkPhyloBranches lvl prox p = setPhyloBranches lvl
                             $ updateGroups Descendant lvl pointers p
  where
    --------------------------------------
    pointers :: Map PhyloGroupId [Pointer]
    pointers = Map.fromList $ map (\(_id,(_id',_w)) -> (_id,[(_id',100)]))
                            $ makeBranchLinks p prox (head' "makeLink" branches) (tail branches) []
    --------------------------------------
    branches :: [(PhyloBranchId,[PhyloGroup])]
    branches = sortOn (\(_id,gs) -> fst $ getFramedPeriod gs) $ getGroupsByBranches p
    -------------------------------------- 




------------------
-- | Branches | --
------------------


-- | To transform a PhyloGraph into a list of PhyloBranches by using the relatedComp clustering
graphToBranches :: Level -> GroupGraph -> Phylo -> [(Int,PhyloGroupId)]
graphToBranches _lvl (nodes,edges) _p = concat
                                    $ map (\(idx,gs) -> map (\g -> (idx,getGroupId g)) gs)
                                    $ zip [1..]
                                    $ relatedComp 0 (head' "branchMaker" nodes) (tail nodes,edges) [] []



-- | To build a graph using the parents and childs pointers
makeGraph :: [PhyloGroup] -> Phylo -> GroupGraph
makeGraph gs p = (gs,edges)
  where 
    edges :: [GroupEdge]
    edges = (nub . concat) 
          $ map (\g -> (map (\g' -> ((g',g),1)) $ getGroupParents g p)
                       ++
                       (map (\g' -> ((g,g'),1)) $ getGroupChilds g p)) gs


-- | To set all the PhyloBranches for a given Level in a Phylo
setPhyloBranches :: Level -> Phylo -> Phylo
setPhyloBranches lvl p = alterGroupWithLevel (\g -> let bIdx = (fst $ head' "branchMaker" $ filter (\b -> snd b == getGroupId g) bs)
                                                     in over (phylo_groupBranchId) (\_ -> Just (lvl,bIdx)) g) lvl p
  where
    --------------------------------------
    bs :: [(Int,PhyloGroupId)]
    bs = graphToBranches lvl graph p
    --------------------------------------
    graph :: GroupGraph
    graph = makeGraph (getGroupsWithLevel lvl p) p
    --------------------------------------
