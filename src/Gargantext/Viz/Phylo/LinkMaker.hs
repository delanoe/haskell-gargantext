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

module Gargantext.Viz.Phylo.LinkMaker
  where

import Control.Parallel.Strategies
import Control.Lens                 hiding (both, Level)
import Data.List                    ((++), sortOn, null, tail, splitAt, elem, concat, delete, intersect, nub, groupBy, union, inits, scanl, find)
import Data.Tuple.Extra
import Data.Map                     (Map,(!),fromListWith,elems,restrictKeys,unionWith,member)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)

-----------------------------
-- | From Level to level | --
-----------------------------


-- | To set the LevelLinks between a given PhyloGroup and a list of childs/parents PhyloGroups
linkGroupToGroups :: PhyloGroup -> [PhyloGroup] -> PhyloGroup
linkGroupToGroups current targets = over (phylo_groupLevelParents) addPointers current
  where
    --------------------------------------
    addPointers :: [Pointer] -> [Pointer]
    addPointers lp = lp ++ Maybe.mapMaybe (\target ->
                                            if (elem (getGroupId current) (getGroupLevelChildsId target))
                                            then Just ((getGroupId target),1)
                                            else Nothing) targets
    --------------------------------------


setLevelLinks :: (Level,Level) -> Phylo -> Phylo
setLevelLinks (lvl,lvl') p = alterGroupWithLevel (\group -> linkGroupToGroups group
                                                           $ filter (\g' -> (not . null) $ intersect (getGroupNgrams group) (getGroupNgrams g'))
                                                           $ getGroupsWithFilters lvl' (getGroupPeriod group) p) lvl p


-------------------------------
-- | From Period to Period | --
-------------------------------


-- | To get the next or previous PhyloPeriod based on a given PhyloPeriodId
getNextPeriods :: Filiation -> Int -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods to' limit id l = case to' of
    Descendant -> take limit $ (tail . snd) next
    Ascendant  -> take limit $ (reverse . fst) next
    _          -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] Filiation type not defined")
    where
      --------------------------------------
      next :: ([PhyloPeriodId], [PhyloPeriodId])
      next = splitAt idx l
      --------------------------------------
      idx :: Int
      idx = case (List.elemIndex id l) of
        Nothing -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] PhyloPeriodId not defined")
        Just i  -> i
      --------------------------------------


-- | To get the number of docs produced during a list of periods
periodsToNbDocs :: [PhyloPeriodId] -> Phylo -> Double
periodsToNbDocs prds phylo = sum $ elems 
                           $ restrictKeys (phylo ^. phylo_docsByYears)
                           $ periodsToYears prds 


-- | To process a given Proximity
processProximity :: Proximity -> Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> [Int] -> [Int] -> Double
processProximity proximity nbDocs cooc cooc' ngrams ngrams' = case proximity of 
  WeightedLogJaccard (WLJParams _ sens) -> weightedLogJaccard sens nbDocs cooc cooc' ngrams ngrams'
  Hamming (HammingParams _)             -> hamming cooc cooc'
  _                                     -> panic "[ERR][Viz.Phylo.LinkMaker.processProximity] Unknown proximity"


filterProximity :: Double -> Proximity -> Bool
filterProximity score prox =  case prox of
  WeightedLogJaccard (WLJParams thr _)   -> score >= thr
  Hamming (HammingParams thr)            -> score <= thr
  _                                      -> panic "[ERR][Viz.Phylo.LinkMaker.filterProximity] Unknown proximity"



makePairs :: [(Date,Date)] -> PhyloGroup -> Phylo -> [(PhyloGroup,PhyloGroup)]
makePairs prds g p = filter (\pair -> ((last' "makePairs" prds) == (getGroupPeriod $ fst pair))
                                    || ((last' "makePairs" prds) == (getGroupPeriod $ snd pair)))
                    $ listToPairs
                    $ filter (\g' -> (elem (getGroupPeriod g') prds)
                                 && ((not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                                 && (((last' "makePairs" prds) == (getGroupPeriod g))
                                     ||((matchWithPairs g (g,g') p) >= (getThreshold $ getPhyloProximity p))))
                    $ getGroupsWithLevel (getGroupLevel g) p 



-- | Find the best candidates to be time-linked with a group g1 (recursively until the limit of periods is reached)
-- | 1) find the next periods and get the mini cooc matrix of g1
-- | 2) build the pairs of candidates (single groups or tuples)
-- | 3) process the proximity mesure and select the best ones to create the pointers (ie: all the max)
findBestCandidates :: Filiation -> Int -> Int -> Proximity -> [(Date,Date)] -> PhyloGroup -> Phylo -> ([Pointer],[Double])
findBestCandidates filiation depth limit proximity periods g1 phylo
  | depth > limit || null nextPeriods = ([],[])
  | (not . null) pointers             = (head' "findBestCandidates" $ groupBy (\x y -> snd x == snd y) pointers
                                        ,map snd similarities)
  | otherwise                         = findBestCandidates filiation (depth + 1) limit proximity periods g1 phylo
  where  
    --------------------------------------
    pointers :: [(PhyloGroupId, Double)]
    pointers = reverse $ sortOn snd $ filter (\(_,score) -> filterProximity score proximity) similarities
    --------------------------------------
    similarities :: [(PhyloGroupId, Double)]
    similarities = concat $ map (\(g2,g3) -> 
      let nbDocs  = periodsToNbDocs [(getGroupPeriod g1),(getGroupPeriod g2),(getGroupPeriod g3)] phylo
          cooc'   = if (g2 == g3)
                      then getGroupCooc g2
                      else unionWith (+) (getGroupCooc g2) (getGroupCooc g3)
          ngrams' = if (g2 == g3)
                      then getGroupNgrams g2
                      else union (getGroupNgrams g2) (getGroupNgrams g3)
          score   = processProximity proximity nbDocs (getGroupCooc g1) cooc' (getGroupNgrams g1) ngrams'
      in  if (g2 == g3)
            then [(getGroupId g2,score)] 
            else [(getGroupId g2,score),(getGroupId g3,score)] ) pairsOfCandidates
    --------------------------------------
    pairsOfCandidates :: [(PhyloGroup,PhyloGroup)]
    pairsOfCandidates = makePairs nextPeriods g1 phylo
    --------------------------------------
    nextPeriods :: [(Date,Date)]
    nextPeriods = take depth periods
    --------------------------------------

matchWithPairs :: PhyloGroup -> (PhyloGroup,PhyloGroup) -> Phylo -> Double
matchWithPairs g1 (g2,g3) p = 
  let nbDocs = periodsToNbDocs [(getGroupPeriod g1),(getGroupPeriod g2),(getGroupPeriod g3)] p
      cooc   = if (g2 == g3)
                then getGroupCooc g2
                else unionWith (+) (getGroupCooc g2) (getGroupCooc g3)
      ngrams = if (g2 == g3)
                then getGroupNgrams g2
                else union (getGroupNgrams g2) (getGroupNgrams g3)
  in processProximity (getPhyloProximity p) nbDocs (getGroupCooc g1) cooc (getGroupNgrams g1) ngrams  


phyloGroupMatching :: [PhyloPeriodId] -> PhyloGroup -> Phylo -> [Pointer]
phyloGroupMatching periods g p = case pointers of
  Nothing  -> []
  Just pts -> head' "phyloGroupMatching"  
            -- | Keep only the best set of pointers grouped by proximity
            $ groupBy (\pt pt' -> snd pt == snd pt') 
            $ reverse $ sortOn snd pts
            -- | Find the first time frame where at leats one pointer satisfies the proximity threshold
  where
    --------------------------------------
    pointers :: Maybe [Pointer]
    pointers = find (not . null)
             -- | For each time frame, process the Proximity on relevant pairs of targeted groups
             $ scanl (\acc frame -> 
                 let pairs = makePairs frame g p
                 in  acc ++ ( filter (\(_,proxi) -> filterProximity proxi (getPhyloProximity p))
                            $ concat 
                            $ map (\(t,t') ->  
                                let proxi = matchWithPairs g (t,t') p
                                in 
                                  if (t == t')
                                    then [(getGroupId t,proxi)]
                                    else [(getGroupId t,proxi),(getGroupId t',proxi)] ) pairs ) ) []
             -- | [[1900],[1900,1901],[1900,1901,1902],...] | length max => + 5 years
             $ inits periods
    --------------------------------------




findBestCandidates' :: Proximity -> [PhyloGroup] -> PhyloGroup -> Phylo -> [Pointer]
findBestCandidates' proximity candidates g1 phylo = pointers
  where  
    --------------------------------------
    pointers :: [(PhyloGroupId, Double)]
    pointers = reverse $ sortOn snd $ filter (\(_,score) -> case proximity of
                  WeightedLogJaccard (WLJParams thr _)   -> score >= (thr - 0.1)
                  Hamming (HammingParams thr)            -> score <= thr
                  _                                      -> panic "[ERR][Viz.Phylo.LinkMaker.findBestCandidates'] Unknown proximity"
                  ) similarities
    --------------------------------------
    similarities :: [(PhyloGroupId, Double)]
    similarities = concat $ map (\(g2,g3) -> let nbDocs  = periodsToNbDocs [(getGroupPeriod g1),(getGroupPeriod g2),(getGroupPeriod g3)] phylo
                                                 cooc'   = unionWith (+) (getGroupCooc g2) (getGroupCooc g3)
                                                 ngrams' = union (getGroupNgrams g2) (getGroupNgrams g3)
                                                 score   = processProximity proximity nbDocs cooc cooc' ngrams ngrams'
                                             in  nub $ [(getGroupId g2,score),(getGroupId g3,score)]) pairsOfCandidates
    --------------------------------------
    pairsOfCandidates :: [(PhyloGroup,PhyloGroup)]
    pairsOfCandidates = listToFullCombi candidates
    --------------------------------------
    --------------------------------------
    cooc :: Map (Int,Int) Double
    cooc = getGroupCooc g1
    --------------------------------------
    ngrams :: [Int]
    ngrams = getGroupNgrams g1
    --------------------------------------             


-- | To add some Pointer to a PhyloGroup
addPointers' :: Filiation -> [Pointer] -> PhyloGroup -> PhyloGroup
addPointers' fil pts g = g & case fil of 
   Descendant -> phylo_groupPeriodChilds  %~ (++ pts)
   Ascendant  -> phylo_groupPeriodParents %~ (++ pts)
   _          -> panic ("[ERR][Viz.Phylo.LinkMaker.addPointers] Wrong type of filiation")



-- | To update a list of phyloGroups with some Pointers
updateGroups :: Filiation -> Level -> Map PhyloGroupId [Pointer] -> Phylo -> Phylo 
updateGroups fil lvl m p = alterPhyloGroups (\gs -> map (\g -> if ((getGroupLevel g) == lvl) && (member (getGroupId g) m)
                                                               then addPointers' fil (m ! (getGroupId g)) g
                                                               else g ) gs) p



-- | Optimisation : to keep only the groups that have at least one ngrams in commons with the target
initCandidates :: PhyloGroup -> [PhyloPeriodId] -> [PhyloGroup] -> [PhyloGroup]
initCandidates g prds gs = filter (\g' -> elem (getGroupPeriod g') prds)
                           $ filter (\g' -> (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                           $ delete g gs


-- | a init avec la [[head groups]] et la tail groups
toBranches :: [[PhyloGroup]] -> [PhyloGroup] -> [[PhyloGroup]]
toBranches mem gs
  | null gs = mem
  | otherwise = toBranches mem' $ tail gs
  where
    --------------------------------------
    mem' :: [[PhyloGroup]]
    mem' = if (null withHead)
           then mem ++ [[head' "toBranches" gs]]
           else (filter (\gs' -> not $ elem gs' withHead) mem)
                ++
                [(concat withHead) ++ [head' "toBranches" gs]]
    --------------------------------------
    withHead :: [[PhyloGroup]]
    withHead = filter (\gs' -> (not . null)
                             $ intersect (concat $ map getGroupNgrams gs')
                                         (getGroupNgrams $ (head' "toBranches" gs))
                      ) mem
    --------------------------------------


-- | To process an intertemporal matching task to a Phylo at a given level
-- | 1) split all groups (of the level) in branches (ie:related components sharing at least one ngram)
-- | 2) for each branch, for each group find the best candidates (by Filiation and Proximity) and create the corresponding pointers
-- | 3) update all the groups with the new pointers if they exist
interTempoMatching :: Filiation -> Level -> Proximity -> Phylo -> Phylo
interTempoMatching fil lvl _ p = updateGroups fil lvl (Map.fromList pointers) p
  where
    --------------------------------------
    -- debug :: [Pointers]
    -- debug = concat $ map (snd) pointers     
    --------------------------------------
    -- pointersMap :: Map PhyloGroupId [Pointer]
    -- pointersMap = Map.fromList $ map (\(id,x) -> (id,fst x)) pointers
    --------------------------------------
    pointers :: [(PhyloGroupId,[Pointer])]
    pointers = 
      let  pts  = map (\g -> let periods = getNextPeriods fil (getPhyloMatchingFrame p) (getGroupPeriod g) (getPhyloPeriods p)
                             in  (getGroupId g, phyloGroupMatching periods g p)) groups
           pts' = pts `using` parList rdeepseq
       in  pts'
    --------------------------------------
    groups :: [PhyloGroup]
    groups = getGroupsWithLevel lvl p
    --------------------------------------


------------------------------------------------------------------------
-- | Make links from Period to Period after level 1

toLevelUp :: [Pointer] -> Phylo -> [Pointer]
toLevelUp lst p = Map.toList 
                $ map (\ws -> maximum ws)
                $ fromListWith (++) [(id, [w]) | (id, w) <- 
                  let pointers  = map (\(id,v) -> (getGroupLevelParentId $ getGroupFromId id p, v)) lst
                      pointers' = pointers `using` parList rdeepseq
                  in  pointers' ]


-- | Transpose the parent/child pointers from one level to another
transposePeriodLinks :: Level -> Phylo -> Phylo
transposePeriodLinks lvl p = alterGroupWithLevel
  (\g ->
    --------------------------------------
    let ascLink = toLevelUp (getGroupPeriodParents g) p 
        desLink = toLevelUp (getGroupPeriodChilds  g) p
    --------------------------------------
    in g & phylo_groupPeriodParents .~ ascLink
         & phylo_groupPeriodChilds  .~ desLink
    --------------------------------------
  ) lvl p 

----------------
-- | Tracer | --
----------------


traceMatching :: Filiation -> Level -> Double -> [Double] -> Phylo -> Phylo
traceMatching fil lvl thr lst p = trace ( "----\n" <> show (fil) <> " unfiltered temporal Matching in Phylo" <> show (lvl) <> " :\n"
                                      <> "count : " <> show (length lst) <> " potential pointers (" <> show (length $ filter (>= thr) lst) <> " >= " <> show (thr) <> ")\n"
                                      <> "similarity : " <> show (percentile 25 (VS.fromList lst)) <> " (25%) "
                                                         <> show (percentile 50 (VS.fromList lst)) <> " (50%) "
                                                         <> show (percentile 75 (VS.fromList lst)) <> " (75%) "
                                                         <> show (percentile 90 (VS.fromList lst)) <> " (90%)\n") p


tracePreBranches :: [[PhyloGroup]] -> [[PhyloGroup]]
tracePreBranches bs = trace (show (length bs) <> " pre-branches" <> "\n"
                             <> "with sizes : " <> show (map length bs) <> "\n") bs

