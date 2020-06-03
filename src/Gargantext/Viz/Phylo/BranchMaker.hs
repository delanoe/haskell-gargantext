{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Viz.Phylo.BranchMaker
  where

import Control.Parallel.Strategies
import Control.Lens     hiding (both, Level)
import Data.List        (concat,nub,(++),sortOn,reverse,sort,null,intersect,union,delete)
import Data.Map         (Map,(!), fromListWith, elems)
import Data.Tuple       (fst, snd)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Cluster
import Gargantext.Viz.Phylo.Aggregates
import Gargantext.Viz.Phylo.Metrics
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


areDistant :: (Date,Date) -> (Date,Date) -> Int -> Bool
areDistant prd prd' thr = (((fst prd') - (snd prd)) > thr) || (((fst prd) - (snd prd')) > thr)


-- | Process a Jaccard on top of two set of Branch Peaks
areTwinPeaks :: Double -> [Int] -> [Int] -> Bool
areTwinPeaks thr ns ns' = ( ((fromIntegral . length) $ intersect ns ns') 
                          / ((fromIntegral . length) $ union ns ns')) >= thr 


-- | Get the framing period of a branch ([[PhyloGroup]])
getBranchPeriod :: [PhyloGroup] -> (Date,Date) 
getBranchPeriod gs =
  let dates = sort $ foldl (\mem g -> mem ++ [fst $ getGroupPeriod g, snd $ getGroupPeriod g]) [] gs
  in  (head' "getBranchPeriod" dates, last' "getBranchPeriod" dates)


-- | Get the Nth most coocurent Ngrams in a list of Groups
getGroupsPeaks :: [PhyloGroup] -> Int -> Phylo -> [Int]
getGroupsPeaks gs nth p = getNthMostOcc nth 
                        $ getSubCooc (getGroupsNgrams gs) 
                        $ getCooc (getGroupsPeriods gs) p


-- | Reduce a list of branches ([[Phylogroup]]) into possible candidates for rebranching
filterSimBranches :: [PhyloGroup] -> Phylo -> [[PhyloGroup]] -> [[PhyloGroup]]
filterSimBranches gs p branches = filter (\gs' -> (areTwinPeaks (getPhyloReBranchThr p)
                                                                (getGroupsPeaks gs  (getPhyloReBranchNth p) p)
                                                                (getGroupsPeaks gs' (getPhyloReBranchNth p)  p))
                                               && ((not . null) $ intersect (map getGroupNgrams gs') (map getGroupNgrams gs))
                                               && (areDistant (getBranchPeriod gs) (getBranchPeriod gs') (getPhyloMatchingFrame p))
                                         ) branches 


-- | Try to connect a focused branch to other candidate branches by finding the best pointers 
reBranch :: Phylo -> [PhyloGroup] -> [[PhyloGroup]] -> [(PhyloGroupId,Pointer)]
reBranch p branch candidates =
  let newLinks  = map (\branch' ->
        let pointers = map (\g -> 
              -- define pairs of candidates groups
              let pairs = listToPairs
                        $ filter (\g' -> (not . null) $ intersect (getGroupNgrams g') (getGroupNgrams g)) branch'
              -- process the matching between the pairs and the current group
              in  foldl' (\mem (g2,g3) -> let s = 0.1 + matchWithPairs g (g2,g3) p
                                          in if (g2 == g3)
                                             then mem ++ [(getGroupId g,(getGroupId g2,s))]
                                             else mem ++ [(getGroupId g,(getGroupId g2,s)),(getGroupId g,(getGroupId g3,s))]) [] pairs
              ) branch
            pointers' = pointers `using` parList rdeepseq
        --  keep the best pointer between the focused branch and the current candidates 
        in head' "reBranch" $ reverse $ sortOn (snd . snd) 
         $ filter (\(_,(_,s)) -> filterProximity s $ getPhyloProximity p) $ concat pointers'
        ) candidates
      newLinks' = newLinks `using` parList rdeepseq
  in  newLinks'


reLinkPhyloBranches :: Level -> Phylo -> Phylo
reLinkPhyloBranches lvl p = 
  let pointers = Map.fromList $ map (\(_id,(_id',_s)) -> (_id,[(_id',100)])) $ fst
               $ foldl' (\(pts,branches') gs -> (pts ++ (reBranch p gs (filterSimBranches gs p branches')), delete gs branches')) 
                        ([],branches) branches
  in setPhyloBranches lvl $ updateGroups Descendant lvl pointers p
  where 
    branches :: [[PhyloGroup]]
    branches = elems 
             $ fromListWith (++) 
             $ foldl' (\mem g -> case getGroupBranchId g of
                                 Nothing -> mem
                                 Just i  -> mem ++ [(i,[g])] )
               [] $ getGroupsWithLevel lvl p


------------------
-- | Branches | --
------------------


-- | To transform a PhyloGraph into a list of PhyloBranches by using the relatedComp clustering
graphToBranches :: [PhyloGroup] -> Map PhyloGroupId Int
graphToBranches groups  = Map.fromList
                         $ concat
                         $ map (\(idx,gIds) -> map (\id -> (id,idx)) gIds)
                         $ zip [1..]
                         $ relatedComp
                         $ map (\g -> [getGroupId g] ++ (getGroupPeriodParentsId g) ++ (getGroupPeriodChildsId g)) groups


-- | To set all the PhyloBranches for a given Level in a Phylo
setPhyloBranches :: Level -> Phylo -> Phylo
setPhyloBranches lvl p = alterGroupWithLevel (\g -> 
  let bIdx = branches ! (getGroupId g)
  in  over (phylo_groupBranchId) (\_ -> Just (lvl,bIdx)) g) lvl p
  where 
    --------------------------------------
    branches :: Map PhyloGroupId Int
    branches = graphToBranches (getGroupsWithLevel lvl p)
    --------------------------------------


-- trace' bs = trace bs
