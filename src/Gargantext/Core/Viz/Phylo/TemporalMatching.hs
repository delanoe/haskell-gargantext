{-|
Module      : Gargantext.Core.Viz.Phylo.TemporalMatching
Description : Module dedicated to the adaptative temporal matching of a Phylo.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
Reference   : Chavalarias, D., Lobbé, Q. & Delanoë, A. Draw me Science. Scientometrics 127, 545–575 (2022). https://doi.org/10.1007/s11192-021-04186-5 
-}

module Gargantext.Core.Viz.Phylo.TemporalMatching where

import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Ord
import Data.List (concat, splitAt, tail, sortOn, sortBy, (++), intersect, null, inits, groupBy, scanl, nub, nubBy, union, dropWhile, partition, or)
import Data.Map  (Map, fromList, elems, restrictKeys, unionWith, findWithDefault, keys, (!), empty, mapKeys, adjust, filterWithKey)
import Debug.Trace (trace)
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Prelude
import Prelude (tan,pi)
import Text.Printf
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vector

type Branch = [PhyloGroup]
type FinalQuality = Double
type LocalQuality = Double
type ShouldTry = Bool


----------------------------
-- | Similarity Measure | --
----------------------------


{- 
-- compute a jaccard similarity between two lists
-}
jaccard :: [Int] -> [Int] -> Double
jaccard inter' union' = ((fromIntegral . length) $ inter') / ((fromIntegral . length) $ union')


{- 
-- process the inverse sumLog
-}
sumInvLog' :: Double -> Double -> [Double] -> Double
sumInvLog' s nb diago = foldl (\mem occ -> mem + (1 / (log (occ + 1/ tan (s * pi / 2)) / log (nb + 1/ tan (s * pi / 2))))) 0 diago


{- 
-- process the sumLog
-}
sumLog' :: Double -> Double -> [Double] -> Double
sumLog' s nb diago = foldl (\mem occ -> mem + (log (occ + 1 / tan (s * pi / 2)) / log (nb + 1/ tan (s * pi / 2)))) 0 diago


{- 
-- compute the weightedLogJaccard
-}
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


{- 
-- compute the weightedLogSim
-- Adapted from Wang, X., Cheng, Q., Lu, W., 2014. Analyzing evolution of research topics with NEViewer: a new method based on dynamic co-word networks. Scientometrics 101, 1253–1271. https://doi.org/10.1007/s11192-014-1347-y (log added in the formula + pair comparison)
-- tests not conclusive
-}
weightedLogSim' :: Double -> Double -> Map Int Double -> [Int] -> [Int] -> Double
weightedLogSim' sens nbDocs diago ego_ngrams target_ngrams
  | null ngramsInter           = 0
  | ngramsInter == ngramsUnion = 1
  | sens == 0    = jaccard ngramsInter ngramsUnion
  | sens > 0     = (sumInvLog' sens nbDocs diagoInter) / minimum [(sumInvLog' sens nbDocs diagoEgo),(sumInvLog' sens nbDocs diagoTarget)]
  | otherwise    = (sumLog' sens nbDocs diagoInter) / minimum [(sumLog' sens nbDocs diagoEgo),(sumLog' sens nbDocs diagoTarget)]
  where
    --------------------------------------
    ngramsInter :: [Int]
    ngramsInter = intersect ego_ngrams target_ngrams
    --------------------------------------
    ngramsUnion :: [Int]
    ngramsUnion = union ego_ngrams target_ngrams
    --------------------------------------
    diagoInter :: [Double]
    diagoInter =  elems $ restrictKeys diago (Set.fromList ngramsInter)
    --------------------------------------
    diagoEgo :: [Double]
    diagoEgo =  elems $ restrictKeys diago (Set.fromList ego_ngrams)
    --------------------------------------
    diagoTarget :: [Double]
    diagoTarget =  elems $ restrictKeys diago (Set.fromList target_ngrams)
    --------------------------------------


{- 
-- perform a seamilarity measure between a given group and a pair of targeted groups
-}
toSimilarity :: Double -> Map Int Double -> PhyloSimilarity -> [Int] -> [Int] -> [Int] -> Double
toSimilarity nbDocs diago similarity egoNgrams targetNgrams targetNgrams' =
  case similarity of
    WeightedLogJaccard sens _ ->
      let pairNgrams = if targetNgrams == targetNgrams'
                          then targetNgrams
                          else union targetNgrams targetNgrams'
       in weightedLogJaccard' sens nbDocs diago egoNgrams pairNgrams
    WeightedLogSim sens _ ->
      let pairNgrams = if targetNgrams == targetNgrams'
                          then targetNgrams
                          else union targetNgrams targetNgrams'
       in weightedLogSim' sens nbDocs diago egoNgrams pairNgrams
    Hamming _ _ -> undefined


-----------------------------
-- | Pointers & Matrices | --
-----------------------------


findLastPeriod :: Filiation -> [Period] -> Period
findLastPeriod fil periods = case fil of
    ToParents -> head' "findLastPeriod" (sortOn fst periods)
    ToChilds  -> last' "findLastPeriod" (sortOn fst periods)
    ToChildsMemory  -> undefined
    ToParentsMemory -> undefined

removeOldPointers :: [Pointer] -> Filiation -> Double -> PhyloSimilarity -> Period
                  -> [((PhyloGroupId,[Int]),(PhyloGroupId,[Int]))]
                  -> [((PhyloGroupId,[Int]),(PhyloGroupId,[Int]))]
removeOldPointers oldPointers fil thr prox prd pairs
  | null oldPointers = pairs
  | null (filterPointers prox thr oldPointers) =
    let lastMatchedPrd = findLastPeriod fil (map (fst . fst . fst) oldPointers)
     in if lastMatchedPrd == prd
        then []
        else filter (\((id,_),(id',_)) ->
                case fil of
                     ToChildsMemory  -> undefined
                     ToParentsMemory -> undefined
                     ToParents -> (((fst . fst . fst) id ) < (fst lastMatchedPrd))
                               || (((fst . fst . fst) id') < (fst lastMatchedPrd))
                     ToChilds  -> (((fst . fst . fst) id ) > (fst lastMatchedPrd))
                               || (((fst . fst . fst) id') > (fst lastMatchedPrd))) pairs
  | otherwise = []

filterPointers :: PhyloSimilarity -> Double -> [Pointer] -> [Pointer]
filterPointers proxi thr pts = filter (\(_,w) -> filterSimilarity proxi thr w) pts

filterPointers' :: PhyloSimilarity -> Double -> [(Pointer,[Int])] -> [(Pointer,[Int])]
filterPointers' proxi thr pts = filter (\((_,w),_) -> filterSimilarity proxi thr w) pts


reduceDiagos :: Map Date Cooc -> Map Int Double
reduceDiagos diagos = mapKeys (\(k,_) -> k)
                    $ foldl (\acc diago -> unionWith (+) acc diago) empty (elems diagos)

filterPointersByPeriod :: Filiation -> [(Pointer,[Int])] -> [Pointer]
filterPointersByPeriod fil pts =
  let pts' = sortOn (fst . fst . fst . fst) pts
      inf  = (fst . fst . fst . fst) $ head' "filterPointersByPeriod" pts'
      sup  = (fst . fst . fst . fst) $ last' "filterPointersByPeriod" pts'
   in map fst
    $ nubBy (\pt pt' -> snd pt == snd pt')
    $ filter (\pt -> ((fst . fst . fst . fst) pt == inf) || ((fst . fst . fst . fst) pt == sup))
    $ case fil of
        ToParents -> reverse pts'
        ToChilds  -> pts'
        ToChildsMemory  -> undefined
        ToParentsMemory -> undefined

filterDocs :: Map Date Double -> [Period] -> Map Date Double
filterDocs d pds = restrictKeys d $ periodsToYears pds

filterDiago :: Map Date Cooc -> [Period] -> Map Date Cooc
filterDiago diago pds = restrictKeys diago $ periodsToYears pds


---------------------------------
-- | Inter-temporal matching | --
---------------------------------


{- 
-- perform the related component algorithm, construct the resulting branch id and update the corresponding group's branch id
-}
groupsToBranches :: Map PhyloGroupId PhyloGroup -> [Branch]
groupsToBranches groups =
    {- run the related component algorithm -}
    let egos = groupBy (\gs gs' -> (fst $ fst $ head' "egos" gs) == (fst $ fst $ head' "egos" gs'))
             $ sortOn  (\gs -> fst $ fst $ head' "egos" gs)
             $ map (\group -> [getGroupId group]
                            ++ (map fst $ group ^. phylo_groupPeriodParents)
                            ++ (map fst $ group ^. phylo_groupPeriodChilds) ) $ elems groups
        --  first find the related components by inside each ego's period
        --  a supprimer
        graph' = map relatedComponents egos
        --  then run it for the all the periods
        branches  = zip [1..]
               $ relatedComponents $ concat (graph' `using` parList rdeepseq)
    --  update each group's branch id
    in map (\(bId,branch) ->
                let groups'  = map (\group -> group & phylo_groupBranchId %~ (\(lvl,lst) -> (lvl,lst ++ [bId])))
                                    $ elems $ restrictKeys groups (Set.fromList branch)
                 in groups' `using` parList rdeepseq 
            ) branches `using` parList rdeepseq 


{- 
-- find the best pair/singleton of parents/childs for a given group
-}
makePairs :: (PhyloGroupId,[Int]) -> [(PhyloGroupId,[Int])] -> [Period] -> [Pointer] -> Filiation -> Double -> PhyloSimilarity
           -> Map Date Double -> Map Date Cooc -> [((PhyloGroupId,[Int]),(PhyloGroupId,[Int]))]
makePairs (egoId, egoNgrams) candidates periods oldPointers fil thr prox docs diagos =
    if (null periods)
        then []
        else removeOldPointers oldPointers fil thr prox lastPrd
           {- at least on of the pair candidates should be from the last added period -}
           $ filter (\((id,_),(id',_)) -> ((fst . fst) id == lastPrd) || ((fst . fst) id' == lastPrd))
           $ filter (\((id,_),(id',_)) -> (elem id inPairs) || (elem id' inPairs)) 
           $ listToCombi' candidates
    where
      --------------------------------------
      inPairs :: [PhyloGroupId]
      inPairs = map fst
                    $ filter (\(id,ngrams) ->
                          let nbDocs = (sum . elems) $ filterDocs docs    ([(fst . fst) egoId, (fst . fst) id])
                              diago  = reduceDiagos  $ filterDiago diagos ([(fst . fst) egoId, (fst . fst) id])
                           in (toSimilarity nbDocs diago prox egoNgrams egoNgrams ngrams) >= thr
                      ) candidates
      --------------------------------------
      lastPrd :: Period
      lastPrd = findLastPeriod fil periods
      --------------------------------------

{- 
-- find the best temporal links between a given group and its parents/childs
-}
phyloGroupMatching :: [[(PhyloGroupId,[Int])]] -> Filiation -> PhyloSimilarity -> Map Date Double -> Map Date Cooc
                   -> Double -> [Pointer] -> (PhyloGroupId,[Int]) -> [Pointer]
phyloGroupMatching candidates filiation proxi docs diagos thr oldPointers (id,ngrams) =
        if (null $ filterPointers proxi thr oldPointers)
          -- if no previous pointers satisfy the current threshold then let's find new pointers
          then if null nextPointers
            then []
            else filterPointersByPeriod filiation
               -- 2) keep only the best set of pointers grouped by Similarity
               $ head' "phyloGroupMatching"
               $ groupBy (\pt pt' -> (snd . fst) pt == (snd . fst) pt')
               -- 1) find the first time frame where at leats one pointer satisfies the Similarity threshold
               $ sortBy (comparing (Down . snd . fst)) $ head' "pointers" nextPointers
        else oldPointers
    where
        nextPointers :: [[(Pointer,[Int])]]
        nextPointers = take 1
                 -- stop as soon as we find a time frame where at least one singleton / pair satisfies the threshold 
                 $ dropWhile (null)
                 -- for each time frame, process the Similarity on relevant pairs of targeted groups
                 $ scanl (\acc targets ->
                            let periods = nub $ map (fst . fst . fst) targets
                                lastPrd = findLastPeriod filiation periods
                                nbdocs  = sum $ elems $ (filterDocs docs ([(fst . fst) id] ++ periods))
                                diago   = reduceDiagos
                                        $ filterDiago diagos ([(fst . fst) id] ++ periods)
                                singletons = processSimilarity nbdocs diago $ map (\g -> (g,g)) $ filter (\g -> (fst . fst . fst) g == lastPrd) targets
                                pairs = makePairs (id,ngrams) targets periods oldPointers filiation thr proxi docs diagos
                            in 
                              if (null singletons) 
                                then acc ++ ( processSimilarity nbdocs diago pairs )
                                else acc ++ singletons
                          ) [] $ map concat $ inits candidates -- groups from [[1900],[1900,1901],[1900,1901,1902],...] 
        -----------------------------
        processSimilarity :: Double -> Map Int Double -> [((PhyloGroupId,[Int]),(PhyloGroupId,[Int]))] -> [(Pointer,[Int])]
        processSimilarity nbdocs diago targets =  filterPointers' proxi thr
                                        $ concat
                                        $ map (\(c,c') ->
                                            let similarity = toSimilarity nbdocs diago proxi ngrams (snd c) (snd c')
                                            in if ((c == c') || (snd c == snd c'))
                                               then [((fst c,similarity),snd c)]
                                               else [((fst c,similarity),snd c),((fst c',similarity),snd c')] ) targets    


{- 
-- get the upstream/downstream timescale of a given period
-}
getNextPeriods :: Filiation -> Int -> Period -> [Period] -> [Period]
getNextPeriods fil max' pId pIds =
    case fil of
        ToChilds  -> take max' $ (tail . snd) $ splitAt (elemIndex' pId pIds) pIds
        ToParents -> take max' $ (reverse . fst) $ splitAt (elemIndex' pId pIds) pIds
        ToChildsMemory  -> undefined
        ToParentsMemory -> undefined


{- 
-- find all the candidates parents/childs of ego
-}
getCandidates :: Int -> PhyloGroup -> [[(PhyloGroupId,[Int])]] -> [[(PhyloGroupId,[Int])]]
getCandidates minNgrams ego targets = 
  if (length (ego ^. phylo_groupNgrams)) > 1
    then  
      map (\groups' -> filter (\g' -> (> minNgrams) $ length $ intersect (ego ^. phylo_groupNgrams) (snd g')) groups') targets
    else 
      map (\groups' -> filter (\g' -> (not . null) $ intersect (ego ^. phylo_groupNgrams) (snd g')) groups') targets


{- 
-- set up and start performing the upstream/downstream inter‐temporal matching period by period
-}
reconstructTemporalLinks :: Int -> [Period] -> PhyloSimilarity -> Double -> Map Date Double -> Map Date Cooc -> [PhyloGroup] -> [PhyloGroup]
reconstructTemporalLinks frame periods similarity thr docs coocs groups =
  let groups' = groupByField _phylo_groupPeriod groups
   in foldl' (\acc prd ->
        let -- 1) find the parents/childs matching periods
            periodsPar = getNextPeriods ToParents frame prd periods
            periodsChi = getNextPeriods ToChilds  frame prd periods
            --  2) find the parents/childs matching candidates
            candidatesPar = map (\prd' -> map (\g -> (getGroupId g, g ^. phylo_groupNgrams)) $ findWithDefault [] prd' groups') periodsPar
            candidatesChi = map (\prd' -> map (\g -> (getGroupId g, g ^. phylo_groupNgrams)) $ findWithDefault [] prd' groups') periodsChi
            --  3) find the parents/childs number of docs by years
            docsPar = filterDocs docs ([prd] ++ periodsPar)
            docsChi = filterDocs docs ([prd] ++ periodsChi)
            --  4) find the parents/child diago by years
            diagoPar = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            diagoChi = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            --  5) match in parallel all the groups (egos) to their possible candidates
            egos  = map (\ego ->
                      let pointersPar = phyloGroupMatching (getCandidates (getMinSharedNgrams similarity) ego candidatesPar) ToParents similarity docsPar diagoPar
                                        thr (getPeriodPointers ToParents ego) (getGroupId ego, ego ^. phylo_groupNgrams)
                          pointersChi = phyloGroupMatching (getCandidates (getMinSharedNgrams similarity) ego candidatesChi) ToChilds  similarity docsChi diagoChi
                                        thr (getPeriodPointers ToChilds  ego) (getGroupId ego, ego ^. phylo_groupNgrams)
                       in addPointers ToChilds  TemporalPointer pointersChi
                        $ addPointers ToParents TemporalPointer pointersPar
                        $ addMemoryPointers ToChildsMemory  TemporalPointer thr pointersChi
                        $ addMemoryPointers ToParentsMemory TemporalPointer thr pointersPar ego)
                  $ findWithDefault [] prd groups'
            egos' = egos `using` parList rdeepseq
         in acc ++ egos'
    ) [] periods


{- 
-- find all the groups matching a list of ngrams
-}
findIdsFromNgrams :: [Int] -> Map Int [PhyloGroupId] -> [PhyloGroupId]
findIdsFromNgrams ngrams roots = nub $ concat $ elems $ filterWithKey (\k _ -> elem k ngrams) roots

formatCandidates :: Filiation -> [PhyloGroup] -> [[(PhyloGroupId,[Int])]]
formatCandidates fil groups = case fil of
  ToChilds  -> map (\groups' -> map (\g -> (getGroupId g, getGroupNgrams g)) groups')
             $ elems
             $ groupByField _phylo_groupPeriod groups
  ToParents -> reverse
             $ map (\groups' -> map (\g -> (getGroupId g, getGroupNgrams g)) groups')
             $ elems
             $ groupByField _phylo_groupPeriod groups
  ToChildsMemory  -> undefined
  ToParentsMemory -> undefined

filterByIds :: PhyloGroupId -> [PhyloGroupId] -> [PhyloGroup] -> [PhyloGroup]
filterByIds egoId ids groups = filter (\g -> ((getGroupId g) /= egoId) && (elem (getGroupId g) ids)) groups

filterByPeriods :: [Period] -> [PhyloGroup] -> [PhyloGroup]
filterByPeriods periods groups = filter (\g -> elem (g ^. phylo_groupPeriod) periods) groups 

filterByNgrams :: Int -> [Int] -> [PhyloGroup] -> [PhyloGroup]
filterByNgrams inf ngrams groups = 
  if (length ngrams) > 1
    then 
      filter (\g -> (> inf) $ length $ intersect (ngrams) (getGroupNgrams g)) groups
    else
      filter (\g -> (not . null) $ intersect (ngrams) (getGroupNgrams g)) groups

{- 
-- perform the upstream/downstream inter‐temporal matching process group by group
-}
reconstructTemporalLinks' :: Int -> [Period] -> PhyloSimilarity -> Double -> Map Date Double -> Map Date Cooc -> Map Int [PhyloGroupId] -> [PhyloGroup] -> [PhyloGroup]
reconstructTemporalLinks' frame periods similarity thr docs coocs roots groups = 
  let egos = map (\ego ->
                let -- 1) find the parents/childs matching periods
                    periodsPar = getNextPeriods ToParents frame (ego ^. phylo_groupPeriod) periods
                    periodsChi = getNextPeriods ToChilds  frame (ego ^. phylo_groupPeriod) periods
                    --  2) find the parents/childs matching candidates
                    candidatesPar = formatCandidates ToParents
                                  $ filterByNgrams (getMinSharedNgrams similarity) (getGroupNgrams ego)
                                  $ filterByPeriods periodsPar
                                  $ filterByIds (getGroupId ego) (findIdsFromNgrams (getGroupNgrams ego) roots) groups
                    candidatesChi = formatCandidates ToChilds
                                  $ filterByNgrams (getMinSharedNgrams similarity) (getGroupNgrams ego)
                                  $ filterByPeriods periodsChi
                                  $ filterByIds (getGroupId ego) (findIdsFromNgrams (getGroupNgrams ego) roots) groups
                    --  3) find the parents/childs number of docs by years
                    docsPar = filterDocs docs ([(ego ^. phylo_groupPeriod)] ++ periodsPar)
                    docsChi = filterDocs docs ([(ego ^. phylo_groupPeriod)] ++ periodsChi)
                    --  4) find the parents/child diago by years
                    diagoPar = filterDiago (map coocToDiago coocs) ([(ego ^. phylo_groupPeriod)] ++ periodsPar)
                    diagoChi = filterDiago (map coocToDiago coocs) ([(ego ^. phylo_groupPeriod)] ++ periodsPar)
                    --  5) match ego to their candidates through time
                    pointersPar = phyloGroupMatching candidatesPar ToParents similarity docsPar diagoPar thr (getPeriodPointers ToParents ego) (getGroupId ego, ego ^. phylo_groupNgrams)
                    pointersChi = phyloGroupMatching candidatesChi ToParents similarity docsChi diagoChi thr (getPeriodPointers ToChilds ego)  (getGroupId ego, ego ^. phylo_groupNgrams)                                                                                        
                in addPointers ToChilds  TemporalPointer pointersChi
                 $ addPointers ToParents TemporalPointer pointersPar
                 $ addMemoryPointers ToChildsMemory  TemporalPointer thr pointersChi
                 $ addMemoryPointers ToParentsMemory TemporalPointer thr pointersPar ego      
              ) groups
  in egos `using` parList rdeepseq



{- 
-- reconstruct a phylomemetic network from a list of groups and from a given threshold
-}
toPhylomemeticNetwork :: Int -> [Period] -> PhyloSimilarity -> Double -> Map Date Double -> Map Date Cooc -> Map Int [PhyloGroupId] -> [PhyloGroup] -> [Branch]
toPhylomemeticNetwork timescale periods similarity thr docs coocs roots groups = 
  groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                   -- $ reconstructTemporalLinks timescale periods similarity thr docs coocs groups
                   $ reconstructTemporalLinks' timescale periods similarity thr docs coocs roots groups


----------------------------
-- | Quality Assessment | --
----------------------------


{- 
-- filter the branches containing x
-}
relevantBranches :: Int -> [Branch] -> [Branch]
relevantBranches x branches =
    filter (\groups -> (any (\group -> elem x $ group ^. phylo_groupNgrams) groups)) branches


{- 
-- compute the accuracy ξ
-- the accuracy of a branch relatively to a root x is computed only over the periods where clusters mentionning x in the phylo do exist
-}
accuracy :: Int -> [(Date,Date)] -> Branch -> Double
accuracy x periods bk  = ((fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) bk') /  (fromIntegral $ length bk'))
  where
    ---
    bk' :: [PhyloGroup]
    bk' = filter (\g -> elem (g ^. phylo_groupPeriod) periods) bk


{- 
-- compute the recall ρ
-}
recall :: Int -> Branch -> [Branch] -> Double
recall x bk bx = ((fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) bk)
               /  (fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) $ concat bx))


{- 
-- compute the F-score function
-}
fScore :: Double -> Int -> [(Date,Date)] -> [PhyloGroup] -> [[PhyloGroup]] -> Double
fScore lambda x periods bk bx =
  let rec = recall x bk bx
      acc = accuracy x periods bk
   in ((1 + lambda ** 2) * acc * rec)
    / (((lambda ** 2) * acc  + rec))


{- 
-- compute the number of groups
-}
wk :: [PhyloGroup] -> Double
wk bk = fromIntegral $ length bk


{- 
-- compute the recall ρ for all the branches
-}
globalRecall :: Map Int Double -> [Branch] -> Double
globalRecall freq branches =
  if (null branches)
    then 0
    else sum
       $ map (\x ->
          let px = freq ! x
              bx = relevantBranches x branches
              wks = sum $ map wk bx
           in (px / pys) * (sum $ map (\bk -> ((wk bk) / wks) * (recall x bk bx)) bx))
       $ keys freq
  where
      pys :: Double
      pys = sum (elems freq)


{- 
-- compute the accuracy ξ for all the branches
-}
globalAccuracy :: Map Int Double -> [Branch] -> Double
globalAccuracy freq branches =
  if (null branches)
    then 0
    else sum
       $ map (\x ->
          let px = freq ! x
              bx = relevantBranches x branches
              -- | periods containing x
              periods = nub $ map _phylo_groupPeriod $ filter (\g -> elem x $ g ^. phylo_groupNgrams) $ concat bx
              wks = sum $ map wk bx
           in (px / pys) * (sum $ map (\bk -> ((wk bk) / wks) * (accuracy x periods bk)) bx))
       $ keys freq
  where
      pys :: Double
      pys = sum (elems freq)


{- 
-- compute the quality score F(λ)
-}
toPhyloQuality :: Double -> Double -> Map Int Double -> [[PhyloGroup]] -> Double
toPhyloQuality fdt lambda freq branches =
  if (null branches)
    then 0
    else sum
       $ map (\x ->
        -- let px = freq ! x
        let bx = relevantBranches x branches
            -- | periods containing x
            periods = nub $ map _phylo_groupPeriod $ filter (\g -> elem x $ g ^. phylo_groupNgrams) $ concat bx
            wks = sum $ map wk bx
              -- in (px / pys) * (sum $ map (\bk -> ((wk bk) / wks) * (fScore beta x bk bx)) bx))
              -- in (1 / fdt) *  (sum $ map (\bk -> ((wk bk) / wks) * (fScore beta x periods bk bx)) bx))
         in (1 / fdt) *  (sum $ map (\bk -> ((wk bk) / wks) * (fScore (tan (lambda * pi / 2)) x periods bk bx)) bx))
       $ keys freq
  -- where
    --  pys :: Double
    --  pys = sum (elems freq)


-------------------------
-- | Sea-level Rise  | --
-------------------------


{- 
-- attach a rise value to branches & groups metadata  
-}
riseToMeta :: Double -> [Branch] -> [Branch]
riseToMeta rise branches =
  let break = length branches > 1
   in map (\b ->
        map (\g ->
          if break then g & phylo_groupMeta .~ (adjust (\lst -> lst ++ [rise]) "breaks"(g ^. phylo_groupMeta))
                   else g) b) branches


{- 
-- attach a thr value to branches & groups metadata  
-}
thrToMeta :: Double -> [Branch] -> [Branch]
thrToMeta thr branches =
  map (\b ->
    map (\g -> g & phylo_groupMeta .~ (adjust (\lst -> lst ++ [thr]) "seaLevels" (g ^. phylo_groupMeta))) b) branches


{- 
-- TODO
-- 1) try the zipper structure https://wiki.haskell.org/Zipper to performe the sea-level rise algorithme
-- 2) investigate how the branches order influences the 'separateBranches' function
-}


{- 
-- sequentially separate each branch for a given threshold and check if it locally increases the quality score
-- sequence = [done] | currentBranch | [rest]
-- done = all the already separated branches
-- rest = all the branches we still have to separate
-}
separateBranches :: Double -> PhyloSimilarity -> Double -> Map Int Double -> Int -> Double -> Double
              -> Int -> Map Date Double -> Map Date Cooc -> Map Int [PhyloGroupId] -> [Period] 
              -> [(Branch,ShouldTry)] -> (Branch,ShouldTry) -> [(Branch,ShouldTry)] 
              -> [(Branch,ShouldTry)]
separateBranches fdt similarity lambda frequency minBranch thr rise timescale docs coocs roots periods done currentBranch rest =
  let done' = done ++ (if snd currentBranch
                        then
                            (if ((null (fst branches')) || (quality > quality'))
                               ----  5) if the quality is not increased by the new branches or if the new branches are all small 
                               ----     then undo the separation and localy stop the sea rise
                               ----     else validate the separation and authorise next sea rise in the long new branches
                               then
                               -- trace ("  ✗ F(λ) = " <> show(quality) <> " (vs) " <> show(quality')
                               --         <> "  | "  <> show(length $ fst ego) <> " groups : "
                               --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                               --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  [(fst currentBranch,False)]
                               else
                               -- trace ("  ✓ F(λ) = " <> show(quality) <> " (vs) " <> show(quality')
                               --         <> "  | "  <> show(length $ fst ego) <> " groups : "
                               --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                               --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  ((map (\e -> (e,True)) (fst branches')) ++ (map (\e -> (e,False)) (snd branches'))))
                        else [currentBranch])
  in
    --  6) if there is no more branch to separate tne return [done'] else continue with [rest]
    if null rest
      then done'
      else separateBranches fdt similarity lambda frequency minBranch thr rise timescale docs coocs roots periods
                       done' (List.head rest) (List.tail rest)
  where
    ------- 1) compute the quality before splitting any branch 
    quality :: LocalQuality
    quality = toPhyloQuality fdt lambda frequency ((map fst done) ++ [fst currentBranch] ++ (map fst rest))

    ------------------- 2) split the current branch and create a new phylomemetic network
    phylomemeticNetwork :: [Branch]
    phylomemeticNetwork = toPhylomemeticNetwork timescale periods similarity thr docs coocs roots (fst currentBranch)
    
    --------- 3) change the new phylomemetic network into a tuple of new branches
    ---------    on the left : the long branches, on the right : the small ones 
    branches' :: ([Branch],[Branch])
    branches' = partition (\b -> (length $ nub $ map _phylo_groupPeriod b) >= minBranch)
              $ thrToMeta thr
              $ riseToMeta rise phylomemeticNetwork
    
    -------- 4) compute again the quality by considering the new branches
    quality' :: LocalQuality
    quality' = toPhyloQuality fdt lambda frequency
               ((map fst done) ++ (fst branches') ++ (snd branches') ++ (map fst rest))


{- 
-- perform the sea-level rise algorithm, browse the similarity ladder and check that we can try out the next step
-}
seaLevelRise :: Double -> PhyloSimilarity -> Double -> Int -> Map Int Double 
             -> [Double] -> Double
             -> Int -> [Period] 
             -> Map Date Double -> Map Date Cooc 
             -> Map Int [PhyloGroupId]
             -> [(Branch,ShouldTry)] 
             -> ([(Branch,ShouldTry)],FinalQuality)
seaLevelRise fdt similarity lambda minBranch frequency ladder rise frame periods docs coocs roots branches =
  -- if the ladder is empty or thr > 1 or there is no branch to break then stop
  if (null ladder) || ((List.head ladder) > 1) || (stopRise branches)
    then (branches, toPhyloQuality fdt lambda frequency (map fst branches))
    else
      -- start breaking up all the possible branches for the current similarity threshold
      let thr = List.head ladder
          branches'  = trace ("threshold = " <> printf "%.3f" thr
                                             <> " F(λ) = " <> printf "%.5f" (toPhyloQuality fdt lambda frequency (map fst branches))
                                             <> " ξ = " <> printf "%.5f" (globalAccuracy frequency (map fst branches))
                                             <> " ρ = " <> printf "%.5f" (globalRecall frequency (map fst branches))
                                             <> " branches = " <> show(length branches))
                     $ separateBranches fdt similarity lambda frequency minBranch thr rise frame docs coocs roots periods
                                     [] (List.head branches)  (List.tail branches)
       in seaLevelRise fdt similarity lambda minBranch frequency (List.tail ladder) (rise + 1) frame periods docs coocs roots branches'
  where 
    --------
    stopRise :: [(Branch,ShouldTry)] -> Bool
    stopRise bs = ((not . or) $ map snd bs)


{- 
-- start the temporal matching process up, recover the resulting branches and update the groups (at scale 1) consequently
-}
temporalMatching :: [Double] -> Phylo -> Phylo
temporalMatching ladder phylo = updatePhyloGroups 1
                         (Map.fromList $ map (\g -> (getGroupId g,g)) $ traceMatchEnd $ concat branches)
                         (updateQuality quality phylo)
  where
    -------
    quality :: FinalQuality
    quality = snd sea

    --------
    branches :: [Branch]
    branches = map fst $ fst sea
    
    ---  2) process the temporal matching by elevating the similarity ladder
    sea :: ([(Branch,ShouldTry)],FinalQuality)
    sea = seaLevelRise (fromIntegral $ Vector.length $ getRoots phylo)
                                (similarity $ getConfig phylo)
                                (getLevel phylo)
                                (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                (getRootsFreq phylo)
                                ladder 1
                                (getTimeFrame $ timeUnit $ getConfig phylo)
                                (getPeriodIds phylo)
                                (getDocsByDate phylo)
                                (getCoocByDate phylo)
                                ((phylo ^. phylo_foundations) ^. foundations_rootsInGroups)
                                (reverse $ sortOn (length . fst) seabed)
    
    ------  1) for each group, process an initial temporal Matching and create a 'seabed'
    ------  ShouldTry determines if you should apply the seaLevelRise function again within each branch
    seabed :: [(Branch,ShouldTry)]
    seabed = map (\b -> (b,(length $ nub $ map _phylo_groupPeriod b) >= (_qua_minBranch $ phyloQuality $ getConfig phylo)))
           $ toPhylomemeticNetwork (getTimeFrame $ timeUnit $ getConfig phylo)
                         (getPeriodIds phylo)
                         (similarity $ getConfig phylo)
                         (List.head ladder)
                         (getDocsByDate phylo)
                         (getCoocByDate phylo)
                         ((phylo ^. phylo_foundations) ^. foundations_rootsInGroups)
                         (traceTemporalMatching $ getGroupsFromScale 1 phylo)