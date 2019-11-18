{-|
Module      : Gargantext.Viz.Phylo.TemporalMatching
Description : Module dedicated to the adaptative temporal matching of a Phylo.
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

module Gargantext.Viz.Phylo.TemporalMatching where

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, groupBy, scanl, nub, union, dropWhile, partition, or)
import Data.Map  (Map, fromList, elems, restrictKeys, unionWith, findWithDefault, keys, (!), singleton, empty, mapKeys)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools

-- import Prelude (logBase)
import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
-- import Debug.Trace (trace)

import qualified Data.Set as Set


-------------------
-- | Proximity | --
-------------------


-- | To compute a jaccard similarity between two lists
jaccard :: [Int] -> [Int] -> Double
jaccard inter' union' = ((fromIntegral . length) $ inter') / ((fromIntegral . length) $ union')


-- | Process the inverse sumLog
sumInvLog' :: Double -> Double -> [Double] -> Double
sumInvLog' s nb diago = foldl (\mem occ -> mem + (1 / (log (occ + s) / log (nb + s)))) 0 diago


-- | Process the sumLog
sumLog' :: Double -> Double -> [Double] -> Double
sumLog' s nb diago = foldl (\mem occ -> mem + (log (occ + s) / log (nb + s))) 0 diago   


weightedLogJaccard' :: Double -> Double -> Map Int Double -> [Int] -> [Int] -> Double
weightedLogJaccard' sens nbDocs diago ngrams ngrams'
  | null ngramsInter           = 0
  | ngramsInter == ngramsUnion = 1
  | sens == 0    = jaccard ngramsInter ngramsUnion
  | sens > 0     = (sumInvLog' sens nbDocs diagoInter) / (sumInvLog' sens nbDocs diagoUnion)
  | otherwise    = (sumLog' sens nbDocs diagoInter) / (sumLog' sens nbDocs diagoUnion)  
  where 
    --------------------------------------
    ngramsInter :: [Int] 
    ngramsInter = intersect ngrams ngrams'   
    --------------------------------------
    ngramsUnion :: [Int] 
    ngramsUnion = union ngrams ngrams'
    --------------------------------------
    diagoInter :: [Double]
    diagoInter =  elems $ restrictKeys diago (Set.fromList ngramsInter)
    --------------------------------------  
    diagoUnion :: [Double]
    diagoUnion =  elems $ restrictKeys diago (Set.fromList ngramsUnion)
    --------------------------------------  


-- | To process the proximity between a current group and a pair of targets group
toProximity :: Double -> Map Int Double -> Proximity -> PhyloGroup -> PhyloGroup -> PhyloGroup -> Double
toProximity nbDocs diago proximity ego target target' =
  case proximity of 
    WeightedLogJaccard sens _ _ -> 
      let targetsNgrams = if target == target'
                          then (target ^. phylo_groupNgrams)
                          else union (target ^. phylo_groupNgrams) (target' ^. phylo_groupNgrams)
       in weightedLogJaccard' sens nbDocs diago (ego ^. phylo_groupNgrams) targetsNgrams
    Hamming -> undefined


------------------------
-- | Local Matching | --
------------------------

findLastPeriod :: Filiation -> [PhyloPeriodId] -> PhyloPeriodId
findLastPeriod fil periods = case fil of
    ToParents -> head' "findLastPeriod" (sortOn fst periods)
    ToChilds  -> last' "findLastPeriod" (sortOn fst periods)


-- | To filter pairs of candidates related to old pointers periods
removeOldPointers :: [Pointer] -> Filiation -> Double -> Proximity -> PhyloPeriodId -> [(PhyloGroup,PhyloGroup)] -> [(PhyloGroup,PhyloGroup)]
removeOldPointers oldPointers fil thr prox prd pairs
  | null oldPointers = pairs
  | null (filterPointers prox thr oldPointers) = 
    let lastMatchedPrd = findLastPeriod fil (map (fst . fst . fst) oldPointers)
     in if lastMatchedPrd == prd
        then []
        else filter (\(g,g') -> 
                case fil of
                     ToParents -> ((fst $ g  ^. phylo_groupPeriod) < (fst lastMatchedPrd))
                               || ((fst $ g' ^. phylo_groupPeriod) < (fst lastMatchedPrd))
                     ToChilds  -> ((fst $ g  ^. phylo_groupPeriod) > (fst lastMatchedPrd))
                               || ((fst $ g' ^. phylo_groupPeriod) > (fst lastMatchedPrd))) pairs 
  | otherwise = []


-- | Find pairs of valuable candidates to be matched
makePairs' :: PhyloGroup -> [PhyloGroup] -> [PhyloPeriodId] -> [Pointer] -> Filiation -> Double -> Proximity -> Map Date Double -> Map Date Cooc -> [(PhyloGroup,PhyloGroup)]
makePairs' ego candidates periods oldPointers fil thr prox docs diagos = 
    case null periods of 
        True  -> []
        False -> removeOldPointers oldPointers fil thr prox lastPrd
               -- | at least on of the pair candidates should be from the last added period 
               $ filter (\(g,g') -> ((g  ^. phylo_groupPeriod) == lastPrd)
                                 || ((g' ^. phylo_groupPeriod) == lastPrd))
               $ listToKeys 
               $ filter (\g -> let nbDocs = sum $ elems $ (filterDocs docs ([ego ^. phylo_groupPeriod, g ^. phylo_groupPeriod]))
                                   diago  = reduceDiagos $ filterDiago diagos ([ego ^. phylo_groupPeriod, g ^. phylo_groupPeriod])
                                in (g ^. phylo_groupPeriod == lastPrd)
                                || ((toProximity nbDocs diago prox ego ego g) >= thr)) candidates 
    where 
        lastPrd :: PhyloPeriodId
        lastPrd = findLastPeriod fil periods


filterPointers :: Proximity -> Double -> [Pointer] -> [Pointer]
filterPointers proxi thr pts = filter (\(_,w) -> filterProximity proxi thr w) pts


reduceDiagos :: Map Date Cooc -> Map Int Double
reduceDiagos diagos = mapKeys (\(k,_) -> k)
                    $ foldl (\acc diago -> unionWith (+) acc diago) empty (elems diagos)


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Proximity -> Map Date Double -> Map Date Cooc -> Double -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil proxi docs diagos thr ego = 
        if (null $ filterPointers proxi thr $ getPeriodPointers fil ego)
          -- | let's find new pointers
          then if null nextPointers
            then addPointers ego fil TemporalPointer []
            else addPointers ego fil TemporalPointer
               $ head' "phyloGroupMatching"
               -- | Keep only the best set of pointers grouped by proximity
               $ groupBy (\pt pt' -> snd pt == snd pt')
               $ reverse $ sortOn snd $ head' "pointers" nextPointers
               -- | Find the first time frame where at leats one pointer satisfies the proximity threshold
          else ego 
    where
        nextPointers :: [[Pointer]]
        nextPointers = take 1
                 $ dropWhile (null)
                 -- | for each time frame, process the proximity on relevant pairs of targeted groups
                 $ scanl (\acc groups ->
                            let periods = nub $ map _phylo_groupPeriod $ concat groups
                                nbdocs  = sum $ elems $ (filterDocs docs ([ego ^. phylo_groupPeriod] ++ periods))
                                diago   = reduceDiagos 
                                        $ filterDiago diagos ([ego ^. phylo_groupPeriod] ++ periods)
                                        -- | important resize nbdocs et diago dans le make pairs
                                pairs = makePairs' ego (concat groups) periods (getPeriodPointers fil ego) fil thr proxi docs diagos
                            in acc ++ ( filterPointers proxi thr 
                                        $ concat
                                        $ map (\(c,c') ->
                                            -- | process the proximity between the current group and a pair of candidates 
                                            let proximity = toProximity nbdocs diago proxi ego c c'
                                            in if (c == c')
                                               then [(getGroupId c,proximity)]
                                               else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs )) []
                 $ inits candidates -- | groups from [[1900],[1900,1901],[1900,1901,1902],...] 


filterDocs :: Map Date Double -> [PhyloPeriodId] -> Map Date Double
filterDocs d pds = restrictKeys d $ periodsToYears pds

filterDiago :: Map Date Cooc -> [PhyloPeriodId] -> Map Date Cooc
filterDiago diago pds = restrictKeys diago $ periodsToYears pds


-----------------------------
-- | Matching Processing | --
-----------------------------


getNextPeriods :: Filiation -> Int -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods fil max' pId pIds = 
    case fil of 
        ToChilds  -> take max' $ (tail . snd) $ splitAt (elemIndex' pId pIds) pIds
        ToParents -> take max' $ (reverse . fst) $ splitAt (elemIndex' pId pIds) pIds


getCandidates :: PhyloGroup -> [[PhyloGroup]] -> [[PhyloGroup]]
getCandidates ego targets = 
  map (\groups' -> 
    filter (\g' -> (not . null) $ intersect (ego ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)
  ) groups') targets


matchGroupsToGroups :: Int -> [PhyloPeriodId] -> Proximity -> Double -> Map Date Double -> Map Date Cooc -> [PhyloGroup] -> [PhyloGroup]
matchGroupsToGroups frame periods proximity thr docs coocs groups =
  let groups' = groupByField _phylo_groupPeriod groups
   in foldl' (\acc prd -> 
        let -- | 1) find the parents/childs matching periods
            periodsPar = getNextPeriods ToParents frame prd periods
            periodsChi = getNextPeriods ToChilds  frame prd periods
            -- | 2) find the parents/childs matching candidates
            candidatesPar = map (\prd' -> findWithDefault [] prd' groups') periodsPar
            candidatesChi = map (\prd' -> findWithDefault [] prd' groups') periodsChi 
            -- | 3) find the parents/child number of docs by years
            docsPar = filterDocs docs ([prd] ++ periodsPar)
            docsChi = filterDocs docs ([prd] ++ periodsChi)
            -- | 4) find the parents/child diago by years
            diagoPar = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            diagoChi = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            -- | 5) match in parallel all the groups (egos) to their possible candidates
            egos  = map (\ego -> phyloGroupMatching (getCandidates ego candidatesPar) ToParents proximity docsPar diagoPar thr
                               $ phyloGroupMatching (getCandidates ego candidatesChi) ToChilds  proximity docsChi diagoChi thr ego)
                  $ findWithDefault [] prd groups'
            egos' = egos `using` parList rdeepseq 
         in acc ++ egos'       
    ) [] periods


-----------------------
-- | Phylo Quality | --
-----------------------


relevantBranches :: Int -> [[PhyloGroup]] -> [[PhyloGroup]]
relevantBranches term branches = 
    filter (\groups -> (any (\group -> elem term $ group ^. phylo_groupNgrams) groups)) branches

fScore :: Double -> Int -> [PhyloGroup] -> [[PhyloGroup]] -> Double
fScore beta i bk bks = 
  let recall = ( (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) bk)
               / (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) $ concat bks))
      accuracy = ( (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) bk)
                 / (fromIntegral $ length bk))
   in ((1 + beta ** 2) * accuracy * recall)
    / (((beta ** 2) * accuracy + recall))


wk :: [PhyloGroup] -> Double
wk bk = fromIntegral $ length bk


toPhyloQuality' :: Double -> Map Int Double -> [[PhyloGroup]] -> Double
toPhyloQuality' beta freq branches =
  if (null branches)
    then 0
    else sum 
       $ map (\i -> 
          let bks = relevantBranches i branches
           in (freq ! i) * (sum $ map (\bk -> ((wk bk) / (sum $ map wk bks)) * (fScore beta i bk bks)) bks))
       $ keys freq


-----------------------------
-- | Adaptative Matching | --
-----------------------------


groupsToBranches :: Map PhyloGroupId PhyloGroup -> [[PhyloGroup]]
groupsToBranches groups =
    -- | run the related component algorithm
    let egos = groupBy (\gs gs' -> (fst $ fst $ head' "egos" gs) == (fst $ fst $ head' "egos" gs'))
             $ sortOn  (\gs -> fst $ fst $ head' "egos" gs)
             $ map (\group -> [getGroupId group] 
                            ++ (map fst $ group ^. phylo_groupPeriodParents)
                            ++ (map fst $ group ^. phylo_groupPeriodChilds) ) $ elems groups
        -- | first find the related components by inside each ego's period
        -- | a supprimer
        graph' = map relatedComponents egos
        -- | then run it for the all the periods
        graph  = zip [1..] 
               $ relatedComponents $ concat (graph' `using` parList rdeepseq)
    -- | update each group's branch id
    in map (\(bId,ids) ->
        let groups'  = map (\group -> group & phylo_groupBranchId %~ (\(lvl,lst) -> (lvl,lst ++ [bId])))
                     $ elems $ restrictKeys groups (Set.fromList ids)
         in groups' `using` parList rdeepseq ) graph


reduceFrequency :: Map Int Double -> [[PhyloGroup]] -> Map Int Double
reduceFrequency frequency branches = 
  restrictKeys frequency (Set.fromList $ (nub . concat) $ map _phylo_groupNgrams $ concat branches)

updateThr :: Double -> [[PhyloGroup]] -> [[PhyloGroup]]
updateThr thr branches = map (\b -> map (\g -> g & phylo_groupMeta .~ (singleton "thr" [thr])) b) branches


-- | Sequentially break each branch of a phylo where
-- done = all the allready broken branches
-- ego  = the current branch we want to break
-- rest = the branches we still have to break
breakBranches :: Proximity -> Double -> Map Int Double -> Int -> Double -> Int -> Map Date Double -> Map Date Cooc -> [PhyloPeriodId] -> [([PhyloGroup],Bool)] -> ([PhyloGroup],Bool) -> [([PhyloGroup],Bool)] -> [([PhyloGroup],Bool)]
breakBranches proximity beta frequency minBranch thr frame docs coocs periods done ego rest =
  -- | 1) keep or not the new division of ego
  let done' = done ++ (if snd ego 
                        then (if ((null (fst ego')) || (quality > quality')) 
                               then
                                -- trace ("  ✗ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                --         <> "  | "  <> show(length $ fst ego) <> " groups : " 
                                --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  [(fst ego,False)] 
                               else
                                -- trace ("  ✓ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                --         <> "  | "  <> show(length $ fst ego) <> " groups : " 
                                --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")     
                                  ((map (\e -> (e,True)) (fst ego')) ++ (map (\e -> (e,False)) (snd ego'))))
                        else [ego])
  in 
    -- | 2) if there is no more branches in rest then return else continue    
    if null rest 
      then done'
      else breakBranches proximity beta frequency minBranch thr frame docs coocs periods
                       done' (head' "breakBranches" rest) (tail' "breakBranches" rest) 
  where
    --------------------------------------
    quality :: Double 
    quality = toPhyloQuality' beta frequency ((map fst done) ++ [fst ego] ++ (map fst rest))
    --------------------------------------
    ego' :: ([[PhyloGroup]],[[PhyloGroup]])
    ego' = 
      let branches  = groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                    $ matchGroupsToGroups frame periods proximity thr docs coocs (fst ego)
          branches' = branches `using` parList rdeepseq
       in partition (\b -> (length $ nub $ map _phylo_groupPeriod b) >= minBranch) 
        $ if (length branches' > 1)
          then updateThr thr branches'
          else branches'    
    --------------------------------------
    quality' :: Double
    quality' = toPhyloQuality' beta frequency
                                    ((map fst done) ++ (fst ego') ++ (snd ego') ++ (map fst rest))


seaLevelMatching :: Proximity -> Double -> Int -> Map Int Double -> Double -> Int -> [PhyloPeriodId] -> Map Date Double -> Map Date Cooc -> [([PhyloGroup],Bool)] -> [([PhyloGroup],Bool)]
seaLevelMatching proximity beta minBranch frequency thr frame periods docs coocs branches =
  -- | if there is no branch to break or if sea level > 1 then end
  if (thr >= 1) || ((not . or) $ map snd branches)
    then branches
    else 
      -- | break all the possible branches at the current sea level
      let branches'  = breakBranches proximity beta frequency minBranch thr frame docs coocs periods 
                                     [] (head' "seaLevelMatching" branches) (tail' "seaLevelMatching" branches)
          frequency' = reduceFrequency frequency (map fst branches')
       in seaLevelMatching proximity beta minBranch frequency' (thr + (getThresholdStep proximity)) frame periods docs coocs branches'


temporalMatching :: Phylo -> Phylo 
temporalMatching phylo = updatePhyloGroups 1 
                         (fromList $ map (\g -> (getGroupId g,g)) $ traceMatchEnd $ concat branches)
                         phylo
  where
    -- | 2) process the temporal matching by elevating sea level      
    branches :: [[PhyloGroup]]
    branches = map fst
             $ seaLevelMatching (phyloProximity $ getConfig phylo)
                                (_qua_granularity $ phyloQuality $ getConfig phylo)
                                (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                (phylo ^. phylo_termFreq)
                                (getThresholdInit $ phyloProximity $ getConfig phylo)
                                (getTimeFrame $ timeUnit $ getConfig phylo)
                                (getPeriodIds phylo)
                                (phylo ^. phylo_timeDocs)
                                (phylo ^. phylo_timeCooc)
                                groups    
    -- | 1) for each group process an initial temporal Matching
    -- | here we suppose that all the groups of level 1 are part of the same big branch
    groups :: [([PhyloGroup],Bool)]
    groups = map (\b -> (b,(length $ nub $ map _phylo_groupPeriod b) >= (_qua_minBranch $ phyloQuality $ getConfig phylo))) 
           $ groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
           $ matchGroupsToGroups (getTimeFrame $ timeUnit $ getConfig phylo) 
                         (getPeriodIds phylo) (phyloProximity $ getConfig phylo) 
                         (getThresholdInit $ phyloProximity $ getConfig phylo) 
                         (phylo ^. phylo_timeDocs) 
                         (phylo ^. phylo_timeCooc)
                         (traceTemporalMatching $ getGroupsFromLevel 1 phylo)