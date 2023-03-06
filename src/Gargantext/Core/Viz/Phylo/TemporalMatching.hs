{-|
Module      : Gargantext.Core.Viz.Phylo.TemporalMatching
Description : Module dedicated to the adaptative temporal matching of a Phylo.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


module Gargantext.Core.Viz.Phylo.TemporalMatching where

import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Ord
import Data.List (concat, splitAt, tail, sortOn, sortBy, (++), intersect, null, inits, groupBy, scanl, nub, nubBy, union, dropWhile, partition, or, sort, (!!))
import Data.Map  (Map, fromList, elems, restrictKeys, unionWith, findWithDefault, keys, (!), (!?), filterWithKey, singleton, empty, mapKeys, adjust)
import Debug.Trace (trace)
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Prelude
import Prelude (floor,tan,pi)
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector


-------------------
-- | Proximity | --
-------------------


-- | To compute a jaccard similarity between two lists
jaccard :: [Int] -> [Int] -> Double
jaccard inter' union' = ((fromIntegral . length) $ inter') / ((fromIntegral . length) $ union')


-- | Process the inverse sumLog
sumInvLog' :: Double -> Double -> [Double] -> Double
sumInvLog' s nb diago = foldl (\mem occ -> mem + (1 / (log (occ + 1/ tan (s * pi / 2)) / log (nb + 1/ tan (s * pi / 2))))) 0 diago


-- | Process the sumLog
sumLog' :: Double -> Double -> [Double] -> Double
sumLog' s nb diago = foldl (\mem occ -> mem + (log (occ + 1/ tan (s * pi / 2)) / log (nb + 1/ tan (s * pi / 2)))) 0 diago


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

-- | Process the weighted similarity between clusters. Adapted from Wang, X., Cheng, Q., Lu, W., 2014. Analyzing evolution of research topics with NEViewer: a new method based on dynamic co-word networks. Scientometrics 101, 1253–1271. https://doi.org/10.1007/s11192-014-1347-y (log added in the formula + pair comparison)
-- tests not conclusive
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

toProximity :: Double -> Map Int Double -> Proximity -> [Int] -> [Int] -> [Int] -> Double
-- | To process the proximity between a current group and a pair of targets group using the adapted Wang et al. Similarity
toProximity nbDocs diago proximity egoNgrams targetNgrams targetNgrams' =
  case proximity of
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

------------------------
-- | Local Matching | --
------------------------

findLastPeriod :: Filiation -> [Period] -> Period
findLastPeriod fil periods = case fil of
    ToParents -> head' "findLastPeriod" (sortOn fst periods)
    ToChilds  -> last' "findLastPeriod" (sortOn fst periods)
    ToChildsMemory  -> undefined
    ToParentsMemory -> undefined


-- | To filter pairs of candidates related to old pointers periods
removeOldPointers :: [Pointer] -> Filiation -> Double -> Proximity -> Period
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



makePairs :: (PhyloGroupId,[Int]) -> [(PhyloGroupId,[Int])] -> [Period] -> [Pointer] -> Filiation -> Double -> Proximity
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
                           in (toProximity nbDocs diago prox egoNgrams egoNgrams ngrams) >= thr
                      ) candidates
      --------------------------------------
      lastPrd :: Period
      lastPrd = findLastPeriod fil periods
      --------------------------------------


filterPointers :: Proximity -> Double -> [Pointer] -> [Pointer]
filterPointers proxi thr pts = filter (\(_,w) -> filterProximity proxi thr w) pts

filterPointers' :: Proximity -> Double -> [(Pointer,[Int])] -> [(Pointer,[Int])]
filterPointers' proxi thr pts = filter (\((_,w),_) -> filterProximity proxi thr w) pts


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


phyloGroupMatching' :: [[(PhyloGroupId,[Int])]] -> Filiation -> Proximity -> Map Date Double -> Map Date Cooc
                   -> Double -> [Pointer] -> (PhyloGroupId,[Int]) -> [Pointer]
phyloGroupMatching' candidates filiation proxi docs diagos thr oldPointers (id,ngrams) =
        if (null $ filterPointers proxi thr oldPointers)
          -- if no previous pointers satisfy the current threshold then let's find new pointers
          then if null nextPointers
            then []
            else filterPointersByPeriod filiation
               -- 2) keep only the best set of pointers grouped by proximity
               $ head' "phyloGroupMatching"
               $ groupBy (\pt pt' -> (snd . fst) pt == (snd . fst) pt')
               -- 1) find the first time frame where at leats one pointer satisfies the proximity threshold
               $ sortBy (comparing (Down . snd . fst)) $ head' "pointers" nextPointers
        else oldPointers
    where
        nextPointers :: [[(Pointer,[Int])]]
        nextPointers = take 1
                 -- stop as soon as we find a time frame where at least one singleton / pair satisfies the threshold 
                 $ dropWhile (null)
                 -- for each time frame, process the proximity on relevant pairs of targeted groups
                 $ scanl (\acc targets ->
                            let periods = nub $ map (fst . fst . fst) targets
                                lastPrd = findLastPeriod filiation periods
                                nbdocs  = sum $ elems $ (filterDocs docs ([(fst . fst) id] ++ periods))
                                diago   = reduceDiagos
                                        $ filterDiago diagos ([(fst . fst) id] ++ periods)
                                singletons = processProximity nbdocs diago $ map (\g -> (g,g)) $ filter (\g -> (fst . fst . fst) g == lastPrd) targets
                                pairs = makePairs (id,ngrams) targets periods oldPointers filiation thr proxi docs diagos
                            in 
                              if (null singletons) 
                                then acc ++ ( processProximity nbdocs diago pairs )
                                else acc ++ singletons
                          ) [] $ map concat $ inits candidates -- groups from [[1900],[1900,1901],[1900,1901,1902],...] 
        -----------------------------
        processProximity :: Double -> Map Int Double -> [((PhyloGroupId,[Int]),(PhyloGroupId,[Int]))] -> [(Pointer,[Int])]
        processProximity nbdocs diago targets =  filterPointers' proxi thr
                                        $ concat
                                        $ map (\(c,c') ->
                                            let proximity = toProximity nbdocs diago proxi ngrams (snd c) (snd c')
                                            in if ((c == c') || (snd c == snd c'))
                                               then [((fst c,proximity),snd c)]
                                               else [((fst c,proximity),snd c),((fst c',proximity),snd c')] ) targets      


phyloGroupMatching :: [[(PhyloGroupId,[Int])]] -> Filiation -> Proximity -> Map Date Double -> Map Date Cooc
                   -> Double -> [Pointer] -> (PhyloGroupId,[Int]) -> [Pointer]
phyloGroupMatching candidates filiation proxi docs diagos thr oldPointers (id,ngrams) =
        if (null $ filterPointers proxi thr oldPointers)
          {- let's find new pointers -}
          then if null nextPointers
            then []
            else filterPointersByPeriod filiation
               $ head' "phyloGroupMatching"
               -- Keep only the best set of pointers grouped by proximity
               $ groupBy (\pt pt' -> (snd . fst) pt == (snd . fst) pt')
               -- verifier que l on garde bien les plus importants
               $ sortBy (comparing (Down . snd . fst)) $ head' "pointers" nextPointers
               -- Find the first time frame where at leats one pointer satisfies the proximity threshold
          else oldPointers
    where
        nextPointers :: [[(Pointer,[Int])]]
        nextPointers = take 1
                 $ dropWhile (null)
                 {- for each time frame, process the proximity on relevant pairs of targeted groups -}
                 $ scanl (\acc groups ->
                            let periods = nub $ map (fst . fst . fst) $ concat groups
                                nbdocs  = sum $ elems $ (filterDocs docs ([(fst . fst) id] ++ periods))
                                diago   = reduceDiagos
                                        $ filterDiago diagos ([(fst . fst) id] ++ periods)
                                pairs = makePairs (id,ngrams) (concat groups) periods oldPointers filiation thr proxi docs diagos
                            in acc ++ ( filterPointers' proxi thr
                                        $ concat
                                        $ map (\(c,c') ->
                                            {- process the proximity between the current group and a pair of candidates -}
                                            let proximity = toProximity nbdocs diago proxi ngrams (snd c) (snd c')
                                            in if ((c == c') || (snd c == snd c'))
                                               then [((fst c,proximity),snd c)]
                                               else [((fst c,proximity),snd c),((fst c',proximity),snd c')] ) pairs )) []
                 $ inits candidates -- groups from [[1900],[1900,1901],[1900,1901,1902],...]


filterDocs :: Map Date Double -> [Period] -> Map Date Double
filterDocs d pds = restrictKeys d $ periodsToYears pds

filterDiago :: Map Date Cooc -> [Period] -> Map Date Cooc
filterDiago diago pds = restrictKeys diago $ periodsToYears pds


-----------------------------
-- | Matching Processing | --
-----------------------------


getNextPeriods :: Filiation -> Int -> Period -> [Period] -> [Period]
getNextPeriods fil max' pId pIds =
    case fil of
        ToChilds  -> take max' $ (tail . snd) $ splitAt (elemIndex' pId pIds) pIds
        ToParents -> take max' $ (reverse . fst) $ splitAt (elemIndex' pId pIds) pIds
        ToChildsMemory  -> undefined
        ToParentsMemory -> undefined


getCandidates :: Int -> PhyloGroup -> [[(PhyloGroupId,[Int])]] -> [[(PhyloGroupId,[Int])]]
getCandidates minNgrams ego targets = 
  if (length (ego ^. phylo_groupNgrams)) > 1
    then  
      map (\groups' -> filter (\g' -> (> minNgrams) $ length $ intersect (ego ^. phylo_groupNgrams) (snd g')) groups') targets
    else 
      map (\groups' -> filter (\g' -> (not . null) $ intersect (ego ^. phylo_groupNgrams) (snd g')) groups') targets


matchGroupsToGroups :: Int -> [Period] -> Proximity -> Double -> Map Date Double -> Map Date Cooc -> [PhyloGroup] -> [PhyloGroup]
matchGroupsToGroups frame periods proximity thr docs coocs groups =
  let groups' = groupByField _phylo_groupPeriod groups
   in foldl' (\acc prd ->
        let -- 1) find the parents/childs matching periods
            periodsPar = getNextPeriods ToParents frame prd periods
            periodsChi = getNextPeriods ToChilds  frame prd periods
            --  2) find the parents/childs matching candidates
            candidatesPar = map (\prd' -> map (\g -> (getGroupId g, g ^. phylo_groupNgrams)) $ findWithDefault [] prd' groups') periodsPar
            candidatesChi = map (\prd' -> map (\g -> (getGroupId g, g ^. phylo_groupNgrams)) $ findWithDefault [] prd' groups') periodsChi
            --  3) find the parents/child number of docs by years
            docsPar = filterDocs docs ([prd] ++ periodsPar)
            docsChi = filterDocs docs ([prd] ++ periodsChi)
            --  4) find the parents/child diago by years
            diagoPar = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            diagoChi = filterDiago (map coocToDiago coocs) ([prd] ++ periodsPar)
            --  5) match in parallel all the groups (egos) to their possible candidates
            egos  = map (\ego ->
                      let pointersPar = phyloGroupMatching' (getCandidates (getMinSharedNgrams proximity) ego candidatesPar) ToParents proximity docsPar diagoPar
                                        thr (getPeriodPointers ToParents ego) (getGroupId ego, ego ^. phylo_groupNgrams)
                          pointersChi = phyloGroupMatching' (getCandidates (getMinSharedNgrams proximity) ego candidatesChi) ToChilds  proximity docsChi diagoChi
                                        thr (getPeriodPointers ToChilds  ego) (getGroupId ego, ego ^. phylo_groupNgrams)
                       in addPointers ToChilds  TemporalPointer pointersChi
                        $ addPointers ToParents TemporalPointer pointersPar
                        $ addMemoryPointers ToChildsMemory  TemporalPointer thr pointersChi
                        $ addMemoryPointers ToParentsMemory TemporalPointer thr pointersPar ego)
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

accuracy :: Int -> [(Date,Date)] -> [PhyloGroup] -> Double
-- The accuracy of a branch relatively to a term x is computed only over the periods there exist some cluster mentionning x in the phylomemy
accuracy x periods bk  = ((fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) bk')
               /  (fromIntegral $ length bk'))
  where
    bk' :: [PhyloGroup]
    bk' = filter (\g -> elem (g ^. phylo_groupPeriod) periods) bk

recall :: Int -> [PhyloGroup] -> [[PhyloGroup]] -> Double
recall x bk bx = ((fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) bk)
               /  (fromIntegral $ length $ filter (\g -> elem x $ g ^. phylo_groupNgrams) $ concat bx))

fScore :: Double -> Int -> [(Date,Date)] -> [PhyloGroup] -> [[PhyloGroup]] -> Double
fScore lambda x periods bk bx =
  let rec = recall x bk bx
      acc = accuracy x periods bk
   in ((1 + lambda ** 2) * acc * rec)
    / (((lambda ** 2) * acc  + rec))


wk :: [PhyloGroup] -> Double
wk bk = fromIntegral $ length bk

toRecall :: Map Int Double -> [[PhyloGroup]] -> Double
toRecall freq branches =
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


toAccuracy :: Map Int Double -> [[PhyloGroup]] -> Double
toAccuracy freq branches =
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


-- | here we do the average of all the local f_scores
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

-- 1 / nb de foundation

------------------------------------
-- | Constant Temporal Matching | --
------------------------------------

-- add a branch id within each group
groupsToBranches :: Map PhyloGroupId PhyloGroup -> [[PhyloGroup]]
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


reduceFrequency :: Map Int Double -> [[PhyloGroup]] -> Map Int Double
reduceFrequency frequency branches =
  restrictKeys frequency (Set.fromList $ (nub . concat) $ map _phylo_groupNgrams $ concat branches)

updateThr :: Double -> [[PhyloGroup]] -> [[PhyloGroup]]
updateThr thr branches = map (\b -> map (\g ->
  g & phylo_groupMeta .~ (singleton "seaLevels" (((g ^. phylo_groupMeta) ! "seaLevels") ++ [thr]))) b) branches


--  Sequentially break each branch of a phylo where
-- done = all the allready broken branches
-- ego  = the current branch we want to break
-- rest = the branches we still have to break
breakBranches :: Double -> Proximity -> Double -> Map Int Double -> Int -> Double -> Double -> Double
              -> Int -> Map Date Double -> Map Date Cooc -> [Period] -> [([PhyloGroup],Bool)] -> ([PhyloGroup],Bool) -> [([PhyloGroup],Bool)] -> [([PhyloGroup],Bool)]
breakBranches fdt proximity lambda frequency minBranch thr depth elevation frame docs coocs periods done ego rest =
  --  1) keep or not the new division of ego
  let done' = done ++ (if snd ego
                        then
                            (if ((null (fst ego')) || (quality > quality'))
                               then
                                -- trace ("  ✗ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                --         <> "  | "  <> show(length $ fst ego) <> " groups : "
                                --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  [(fst ego,False)]
                               else
                                -- trace ("  ✓ level = " <> printf "%.1f" thr <> "")
                                -- trace ("  ✓ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                --         <> "  | "  <> show(length $ fst ego) <> " groups : "
                                --         <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                --         <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  ((map (\e -> (e,True)) (fst ego')) ++ (map (\e -> (e,False)) (snd ego'))))
                        else [ego])
  in
    --  2) if there is no more branches in rest then return else continue
    if null rest
      then done'
      else breakBranches fdt proximity lambda frequency minBranch thr depth elevation frame docs coocs periods
                       done' (head' "breakBranches" rest) (tail' "breakBranches" rest)
  where
    --------------------------------------
    quality :: Double
    quality = toPhyloQuality fdt lambda frequency ((map fst done) ++ [fst ego] ++ (map fst rest))
    --------------------------------------
    ego' :: ([[PhyloGroup]],[[PhyloGroup]])
    ego' =
      let branches  = groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                    $ matchGroupsToGroups frame periods proximity thr docs coocs (fst ego)
          branches' = branches `using` parList rdeepseq
       in partition (\b -> (length $ nub $ map _phylo_groupPeriod b) >= minBranch)
        $ thrToMeta thr
        $ depthToMeta (elevation - depth) branches'
    --------------------------------------
    quality' :: Double
    quality' = toPhyloQuality fdt lambda frequency
                                    ((map fst done) ++ (fst ego') ++ (snd ego') ++ (map fst rest))


seaLevelMatching :: Double -> Proximity -> Double -> Int -> Map Int Double -> Double -> Double -> Double -> Double
                 -> Int -> [Period] -> Map Date Double -> Map Date Cooc -> [([PhyloGroup],Bool)] -> ([([PhyloGroup],Bool)],Double)
seaLevelMatching fdt proximity lambda minBranch frequency thr step depth elevation frame periods docs coocs branches =
  --  if there is no branch to break or if seaLvl level > 1 then end
  if (thr >= 1) || ((not . or) $ map snd branches)
    then (branches, toPhyloQuality fdt lambda frequency (map fst branches))
    else
      -- break all the possible branches at the current seaLvl level
      let quality    = toPhyloQuality fdt lambda frequency (map fst branches)
          acc        = toAccuracy frequency (map fst branches)
          rec        = toRecall frequency (map fst branches)
          branches'  = trace ("↑ level = " <> printf "%.3f" thr <> " F(λ) = " <> printf "%.5f" quality
                                                                <> " ξ = " <> printf "%.5f" acc
                                                                <> " ρ = " <> printf "%.5f" rec
                                                                <> " branches = " <> show(length branches) <> " ↴")
                     $ breakBranches fdt proximity lambda frequency minBranch thr depth elevation frame docs coocs periods
                                     [] (head' "seaLevelMatching" branches) (tail' "seaLevelMatching" branches)
          frequency' = reduceFrequency frequency (map fst branches')
       in seaLevelMatching fdt proximity lambda minBranch frequency' (thr + step) step (depth - 1) elevation frame periods docs coocs branches'


constanteTemporalMatching :: Double -> Double -> Phylo -> Phylo
constanteTemporalMatching start step phylo = updatePhyloGroups 1
                         (fromList $ map (\g -> (getGroupId g,g)) $ traceMatchEnd $ concat (map fst $ (fst branches)))
                         (toPhyloHorizon (updateQuality (snd branches) phylo))
  where
    --  2) process the temporal matching by elevating seaLvl level
    -- branches :: ([([groups in the same branch],should westill break the branch?)],final quality)
    branches :: ([([PhyloGroup],Bool)],Double)
    branches = seaLevelMatching (fromIntegral $ Vector.length $ getRoots phylo)
                                (phyloProximity $ getConfig phylo)
                                (_qua_granularity $ phyloQuality $ getConfig phylo)
                                (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                (phylo ^. phylo_termFreq)
                                start step
                                ((((1 - start) / step) - 1))
                                (((1 - start) / step))
                                (getTimeFrame $ timeUnit $ getConfig phylo)
                                (getPeriodIds phylo)
                                (phylo ^. phylo_timeDocs)
                                (phylo ^. phylo_timeCooc)
                                (reverse $ sortOn (length . fst) initBranches)
    --  1) for each group process an initial temporal Matching
    --  here we suppose that all the groups of level 1 are part of the same big branch
    --  the Bool param determines weither you should apply the sealevel within the branch
    -- creer un type [PhyloGroup] <=> Branch
    initBranches :: [([PhyloGroup],Bool)]
    initBranches = map (\b -> (b,(length $ nub $ map _phylo_groupPeriod b) >= (_qua_minBranch $ phyloQuality $ getConfig phylo)))
           $ groupsToBranches $ Map.fromList $ map (\g -> (getGroupId g, g))
           $ matchGroupsToGroups (getTimeFrame $ timeUnit $ getConfig phylo)
                         (getPeriodIds phylo) (phyloProximity $ getConfig phylo)
                         start
                         (phylo ^. phylo_timeDocs)
                         (phylo ^. phylo_timeCooc)
                         (traceTemporalMatching $ getGroupsFromLevel 1 phylo)

-----------------
-- | Horizon | --
-----------------

toPhyloHorizon :: Phylo -> Phylo
toPhyloHorizon phylo =
  let t0 = take 1 (getPeriodIds phylo)
      groups = getGroupsFromLevelPeriods 1 t0 phylo
      sens = getSensibility (phyloProximity $ getConfig phylo)
      nbDocs = sum $ elems $ filterDocs (phylo ^. phylo_timeDocs) t0
      diago = reduceDiagos $ filterDiago (phylo ^. phylo_timeCooc) t0
   in phylo & phylo_horizon .~ (fromList $ map (\(g,g') ->
        ((getGroupId g,getGroupId g'),weightedLogJaccard' sens nbDocs diago (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))) $ listToCombi' groups)


--------------------------------------
-- | Adaptative Temporal Matching | --
--------------------------------------


thrToMeta :: Double -> [[PhyloGroup]] -> [[PhyloGroup]]
thrToMeta thr branches =
  map (\b ->
    map (\g -> g & phylo_groupMeta .~ (adjust (\lst -> lst ++ [thr]) "seaLevels" (g ^. phylo_groupMeta))) b) branches

depthToMeta :: Double -> [[PhyloGroup]] -> [[PhyloGroup]]
depthToMeta depth branches =
  let break = length branches > 1
   in map (\b ->
        map (\g ->
          if break then g & phylo_groupMeta .~ (adjust (\lst -> lst ++ [depth]) "breaks"(g ^. phylo_groupMeta))
                   else g) b) branches

reduceTupleMapByKeys :: Eq a => [a] -> Map (a,a) Double -> Map (a,a) Double
reduceTupleMapByKeys ks m = filterWithKey (\(k,k') _ -> (elem k ks) && (elem k' ks)) m


getInTupleMap :: Ord a => Map (a,a) Double -> a -> a -> Double
getInTupleMap m k k'
  | isJust (m !? ( k ,k')) = m ! ( k ,k')
  | isJust (m !? ( k',k )) = m ! ( k',k )
  | otherwise = 0


toThreshold :: Double -> Map (PhyloGroupId,PhyloGroupId) Double -> Double
toThreshold nbSteps proxiGroups =
  let idx = ((Map.size proxiGroups) `div` (floor nbSteps)) - 1
   in if idx >= 0
        then (sort $ elems proxiGroups) !! idx
        else 1


-- done = all the allready broken branches
-- ego  = the current branch we want to break
-- rest = the branches we still have to break
adaptativeBreakBranches :: Double -> Proximity -> Double -> Double -> Map (PhyloGroupId,PhyloGroupId) Double
               -> Double -> Map Int Double -> Int -> Int -> Map Date Double -> Map Date Cooc
               -> [Period] -> [([PhyloGroup],(Bool,[Double]))] -> ([PhyloGroup],(Bool,[Double])) -> [([PhyloGroup],(Bool,[Double]))]
               -> [([PhyloGroup],(Bool,[Double]))]
adaptativeBreakBranches fdt  proxiConf depth elevation groupsProxi lambda frequency minBranch frame docs coocs periods done ego rest =
  --  1) keep or not the new division of ego
  let done' = done ++ (if (fst . snd) ego
                        then (if ((null (fst ego')) || (quality > quality'))
                               then
                                  [(concat $ thrToMeta thr $ [fst ego],(False, ((snd . snd) ego)))]
                               else
                                  (  (map (\e -> (e,(True,  ((snd . snd) ego) ++ [thr]))) (fst ego'))
                                  ++ (map (\e -> (e,(False, ((snd . snd) ego)))) (snd ego'))))
                        else [(concat $ thrToMeta thr $ [fst ego], snd ego)])
  in
    --  uncomment let .. in for debugging
    -- let part1 = partition (snd) done'
    --     part2 = partition (snd) rest
    --  in trace ( "[✓ " <> show(length $ fst part1) <> "(" <> show(length $ concat $ map (fst) $ fst part1) <> ")|✗ " <> show(length $ snd part1) <> "(" <> show(length $ concat $ map (fst) $ snd part1) <> ")] "
    --          <> "[✓ " <> show(length $ fst part2) <> "(" <> show(length $ concat $ map (fst) $ fst part2) <> ")|✗ " <> show(length $ snd part2) <> "(" <> show(length $ concat $ map (fst) $ snd part2) <> ")]"
    --            ) $
    --  2) if there is no more branches in rest then return else continue
    if null rest
      then done'
      else adaptativeBreakBranches fdt proxiConf depth elevation groupsProxi lambda frequency minBranch frame docs coocs periods
                       done' (head' "breakBranches" rest) (tail' "breakBranches" rest)
  where
    --------------------------------------
    thr :: Double
    thr = toThreshold depth $ Map.filter (\v -> v > (last' "breakBranches" $ (snd . snd) ego)) $ reduceTupleMapByKeys (map getGroupId $ fst ego) groupsProxi
    --------------------------------------
    quality :: Double
    quality = toPhyloQuality fdt lambda frequency ((map fst done) ++ [fst ego] ++ (map fst rest))
    --------------------------------------
    ego' :: ([[PhyloGroup]],[[PhyloGroup]])
    ego' =
      let branches  = groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                    $ matchGroupsToGroups frame periods proxiConf thr docs coocs (fst ego)
          branches' = branches `using` parList rdeepseq
       in partition (\b -> (length $ nub $ map _phylo_groupPeriod b) > minBranch)
        $ thrToMeta thr
        $ depthToMeta (elevation - depth) branches'
    --------------------------------------
    quality' :: Double
    quality' = toPhyloQuality fdt lambda frequency
                                    ((map fst done) ++ (fst ego') ++ (snd ego') ++ (map fst rest))


adaptativeSeaLevelMatching :: Double -> Proximity -> Double -> Double -> Map (PhyloGroupId, PhyloGroupId) Double
                  -> Double -> Int -> Map Int Double
                  -> Int -> [Period] -> Map Date Double -> Map Date Cooc
                  -> [([PhyloGroup],(Bool,[Double]))] -> [([PhyloGroup],(Bool,[Double]))]
adaptativeSeaLevelMatching fdt proxiConf depth elevation groupsProxi lambda minBranch frequency frame periods docs coocs branches =
  --  if there is no branch to break or if seaLvl level >= depth then end
  if (Map.null groupsProxi) || (depth <= 0) || ((not . or) $ map (fst . snd) branches)
    then branches
    else
      --  break all the possible branches at the current seaLvl level
      let branches'  = adaptativeBreakBranches fdt proxiConf depth elevation groupsProxi lambda frequency minBranch frame docs coocs periods
                                      [] (head' "seaLevelMatching" branches) (tail' "seaLevelMatching" branches)
          frequency' = reduceFrequency frequency (map fst branches')
          groupsProxi' = reduceTupleMapByKeys (map (getGroupId) $ concat $ map (fst) $ filter (fst . snd) branches') groupsProxi
          thr =  toThreshold depth groupsProxi
       in trace("\n  " <> foldl (\acc _ -> acc <> "🌊 ") "" [0..(elevation - depth)]
                       <> " [✓ " <> show(length $ filter (fst . snd) branches') <> "(" <> show(length $ concat $ map (fst) $ filter (fst . snd) branches')
                       <> ")|✗ " <> show(length $ filter (not . fst . snd) branches') <> "(" <> show(length $ concat $ map (fst) $ filter (not . fst . snd) branches') <> ")]"
                       <> " thr = " <> show(thr))
        $ adaptativeSeaLevelMatching fdt proxiConf (depth - 1) elevation groupsProxi' lambda minBranch frequency' frame periods docs coocs branches'


adaptativeTemporalMatching :: Double -> Phylo -> Phylo
adaptativeTemporalMatching elevation phylo = updatePhyloGroups 1
                          (fromList $ map (\g -> (getGroupId g,g)) $ traceMatchEnd $ concat branches)
                          (toPhyloHorizon phylo)
  where
    --  2) process the temporal matching by elevating seaLvl level
    branches :: [[PhyloGroup]]
    branches = map fst
             $ adaptativeSeaLevelMatching (fromIntegral $ Vector.length $ getRoots phylo)
                                 (phyloProximity $ getConfig phylo)
                                 (elevation - 1)
                                 elevation
                                 (phylo ^. phylo_groupsProxi)
                                 (_qua_granularity $ phyloQuality $ getConfig phylo)
                                 (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                 (phylo ^. phylo_termFreq)
                                 (getTimeFrame $ timeUnit $ getConfig phylo)
                                 (getPeriodIds phylo)
                                 (phylo ^. phylo_timeDocs)
                                 (phylo ^. phylo_timeCooc)
                                 groups
    --  1) for each group process an initial temporal Matching
    --  here we suppose that all the groups of level 1 are part of the same big branch
    groups :: [([PhyloGroup],(Bool,[Double]))]
    groups = map (\b -> (b,((length $ nub $ map _phylo_groupPeriod b) >= (_qua_minBranch $ phyloQuality $ getConfig phylo),[thr])))
           $ groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
           $ matchGroupsToGroups (getTimeFrame $ timeUnit $ getConfig phylo)
                         (getPeriodIds phylo) (phyloProximity $ getConfig phylo)
                         thr
                         (phylo ^. phylo_timeDocs)
                         (phylo ^. phylo_timeCooc)
                         (traceTemporalMatching $ getGroupsFromLevel 1 phylo)
    --------------------------------------
    thr :: Double
    thr = toThreshold elevation (phylo ^. phylo_groupsProxi)
