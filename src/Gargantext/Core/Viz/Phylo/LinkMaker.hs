{-|
Module      : Gargantext.Core.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Core.Viz.Phylo.LinkMaker
  where

import Control.Parallel.Strategies
import Control.Lens                 hiding (both, Level)
import Data.List                    ((++), sortOn, null, tail, splitAt, concat, delete, intersect, elemIndex, groupBy, union, inits, scanl, find)
import Data.Tuple.Extra
import Data.Map                     (Map, (!), fromListWith, elems, restrictKeys, filterWithKey, keys, unionWith, unions, intersectionWith, member, fromList)
import Gargantext.Prelude
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.Tools
import Gargantext.Core.Viz.Phylo.Metrics
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
                                     ||((matchWithPairs g (g,g') p) >= (getPhyloMatchingFrameTh p))))
                    $ getGroupsWithLevel (getGroupLevel g) p 

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
            -- Keep only the best set of pointers grouped by proximity
            $ groupBy (\pt pt' -> snd pt == snd pt') 
            $ reverse $ sortOn snd pts
            -- Find the first time frame where at leats one pointer satisfies the proximity threshold
  where
    --------------------------------------
    pointers :: Maybe [Pointer]
    pointers = find (not . null)
             -- For each time frame, process the Proximity on relevant pairs of targeted groups
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
             -- [[1900],[1900,1901],[1900,1901,1902],...] | length max => + 5 years
             $ inits periods
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
listToTuple :: (a -> b) -> [a] -> [(b,a)]
listToTuple f l = map (\x -> (f x, x)) l


groupsToMaps :: Ord b => (PhyloGroup -> b) -> [PhyloGroup] -> [Map PhyloGroupId PhyloGroup]
groupsToMaps f gs = map (\gs' -> fromList $ listToTuple getGroupId gs')
                  $ groupBy ((==) `on` f)
                  $ sortOn f gs


phyloToPeriodMaps :: Level -> Filiation -> Phylo -> [Map PhyloGroupId PhyloGroup]
phyloToPeriodMaps lvl fil p =
  let prdMap = groupsToMaps (fst . getGroupPeriod) (getGroupsWithLevel lvl p)
  in case fil of  
    Ascendant  -> reverse prdMap
    Descendant -> prdMap
    _          -> panic ("[ERR][Viz.Phylo.LinkMaker.phyloToPeriodMaps] Wrong type of filiation")


trackPointersRec :: Filiation -> Map PhyloGroupId PhyloGroup -> [PhyloGroup] -> [PhyloGroup] -> [PhyloGroup]
trackPointersRec fil m gs res =
  if (null gs) then res
  else if (Map.null m) then res ++ gs
  else 
    let g = head' "track" gs
        pts  = Map.fromList $ getGroupPointers PeriodEdge fil g
        pts' = Map.toList $ fromListWith (\w w' -> max w w') $ concat $ elems
             $ intersectionWith (\w g' -> map (\(id,_w') -> (id, w))
                                        $ getGroupPointers LevelEdge Ascendant g') pts m
        res' = res ++ [case fil of 
                        Ascendant  -> g & phylo_groupPeriodParents .~ pts' 
                        Descendant -> g & phylo_groupPeriodChilds  .~ pts'
                        _          -> panic ("[ERR][Viz.Phylo.LinkMaker.transposeLinks] Wrong type of filiation")]
    in trackPointersRec fil (filterWithKey (\k _ -> not $ elem k (keys pts)) m) (tail' "track" gs) res'



transposeLinks :: Level -> Filiation -> Phylo -> Phylo
transposeLinks lvl fil p = 
  let prdMap = zip (phyloToPeriodMaps (lvl - 1) fil p) (phyloToPeriodMaps lvl fil p)
      transposed =  map (\(gs,gs') -> 
                            let idx  = fromJust $ elemIndex (gs,gs') prdMap
                                next = take (getPhyloMatchingFrame p) $ snd $ splitAt (idx + 1) prdMap
                                groups = trackPointersRec fil (unions $ map fst next) (elems gs') []
                            in  (getGroupPeriod $ head' "transpose" groups ,groups)
                         ) prdMap
      transposed' = Map.fromList $ (transposed `using` parList rdeepseq)
  in alterPhyloGroups
      (\gs -> if ((not . null) gs) && (lvl == (getGroupLevel $ head' "transpose" gs))
              then transposed' ! (getGroupPeriod $ head' "transpose" gs)
              else gs
      ) p 



-- | Transpose the parent/child pointers from one level to another
transposePeriodLinks :: Level -> Phylo -> Phylo
transposePeriodLinks lvl p = alterPhyloGroups
  (\gs -> if ((not . null) gs) && (elem lvl $ map getGroupLevel gs)  
              then 
                let groups  = map (\g -> let m = reduceGroups g lvlGroups
                                         in  g & phylo_groupPeriodParents .~ (trackPointers m $ g ^. phylo_groupPeriodParents)
                                               & phylo_groupPeriodChilds  .~ (trackPointers m $ g ^. phylo_groupPeriodChilds )) gs
                    groups' = groups `using` parList rdeepseq
                    in groups'
              else gs
  ) p
  where
    --------------------------------------
    -- | find an other way to find the group from the id
    trackPointers :: Map PhyloGroupId PhyloGroup -> [Pointer] -> [Pointer]
    trackPointers m pts = Map.toList
                        $ fromListWith (\w w' -> max w w')
                        $ map (\(id,_w) -> (getGroupLevelParentId $ m ! id,_w)) pts
    --------------------------------------                  
    reduceGroups :: PhyloGroup -> [PhyloGroup] -> Map PhyloGroupId PhyloGroup
    reduceGroups g gs = Map.fromList
                     $ map (\g' -> (getGroupId g',g')) 
                     $ filter (\g' -> ((not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))) gs 
    --------------------------------------
    lvlGroups :: [PhyloGroup]
    lvlGroups = getGroupsWithLevel (lvl - 1) p
    --------------------------------------


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

