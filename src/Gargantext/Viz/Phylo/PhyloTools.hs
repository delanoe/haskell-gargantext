{-|
Module      : Gargantext.Viz.Phylo.PhyloTools
Description : Module dedicated to all the tools needed for making a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Viz.Phylo.PhyloTools where

import Data.Vector (Vector, elemIndex)
import Data.List (sort, concat, null, union, (++), tails, sortOn, nub, init, tail, partition, tails, nubBy)
import Data.Set (Set, size, disjoint)
import Data.Map (Map, elems, fromList, unionWith, keys, member, (!), filterWithKey, fromListWith, empty)
import Data.String (String)
import Data.Text (Text, unwords)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Text.Printf


import Debug.Trace (trace)
import Control.Lens hiding (Level)

import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

------------
-- | Io | --
------------

-- | To print an important message as an IO()
printIOMsg :: String -> IO ()
printIOMsg msg = 
    putStrLn ( "\n"
            <> "------------" 
            <> "\n"
            <> "-- | " <> msg <> "\n" )


-- | To print a comment as an IO()
printIOComment :: String -> IO ()
printIOComment cmt =
    putStrLn ( "\n" <> cmt <> "\n" )


--------------
-- | Misc | --
--------------


roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"


countSup :: Double -> [Double] -> Int
countSup s l = length $ filter (>s) l


dropByIdx :: Int -> [a] -> [a]
dropByIdx k l = take k l ++ drop (k+1) l


elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' e l = case (List.elemIndex e l) of
    Nothing -> panic ("[ERR][Viz.Phylo.PhyloTools] element not in list")
    Just i  -> i


commonPrefix :: Eq a => [a] -> [a] -> [a] -> [a]
commonPrefix lst lst' acc =
    if (null lst || null lst')
        then acc
        else if (head' "commonPrefix" lst == head' "commonPrefix" lst')
                then commonPrefix (tail lst) (tail lst') (acc ++ [head' "commonPrefix" lst])
                else acc


---------------------
-- | Foundations | --
---------------------


-- | Is this Ngrams a Foundations Root ?
isRoots :: Ngrams -> Vector Ngrams -> Bool
isRoots n ns = Vector.elem n ns

-- | To transform a list of nrams into a list of foundation's index
ngramsToIdx :: [Ngrams] -> Vector Ngrams -> [Int]
ngramsToIdx ns fdt = map (\n -> fromJust $ elemIndex n fdt) ns

-- | To transform a list of Ngrams Indexes into a Label
ngramsToLabel :: Vector Ngrams -> [Int] -> Text
ngramsToLabel ngrams l = unwords $ tail' "ngramsToLabel" $ concat $ map (\n -> ["|",n]) $ ngramsToText ngrams l


-- | To transform a list of Ngrams Indexes into a list of Text
ngramsToText :: Vector Ngrams -> [Int] -> [Text]
ngramsToText ngrams l = map (\idx -> ngrams Vector.! idx) l


--------------
-- | Time | --
--------------

-- | To transform a list of periods into a set of Dates
periodsToYears :: [(Date,Date)] -> Set Date
periodsToYears periods = (Set.fromList . sort . concat)
                       $ map (\(d,d') -> [d..d']) periods


findBounds :: [Date] -> (Date,Date)
findBounds dates = 
    let dates' = sort dates
    in  (head' "findBounds" dates', last' "findBounds" dates')


toPeriods :: [Date] -> Int -> Int -> [(Date,Date)]
toPeriods dates p s = 
    let (start,end) = findBounds dates
    in map (\dates' -> (head' "toPeriods" dates', last' "toPeriods" dates')) 
     $ chunkAlong p s [start .. end]


-- | Get a regular & ascendante timeScale from a given list of dates
toTimeScale :: [Date] -> Int -> [Date]
toTimeScale dates step = 
    let (start,end) = findBounds dates
    in  [start, (start + step) .. end]


getTimeStep :: TimeUnit -> Int
getTimeStep time = case time of 
    Year _ s _ -> s

getTimePeriod :: TimeUnit -> Int
getTimePeriod time = case time of 
    Year p _ _ -> p  

getTimeFrame :: TimeUnit -> Int
getTimeFrame time = case time of 
    Year _ _ f -> f

-------------
-- | Fis | --
-------------


-- | To find if l' is nested in l
isNested :: Eq a => [a] -> [a] -> Bool
isNested l l'
  | null l'               = True
  | length l' > length l  = False
  | (union  l l') == l    = True
  | otherwise             = False 


-- | To filter Fis with small Support but by keeping non empty Periods
keepFilled :: (Int -> [a] -> [a]) -> Int -> [a] -> [a]
keepFilled f thr l = if (null $ f thr l) && (not $ null l)
                     then keepFilled f (thr - 1) l
                     else f thr l


traceClique :: Map (Date, Date) [PhyloClique] -> String
traceClique mFis = foldl (\msg cpt -> msg <> show (countSup cpt cliques) <> " (>" <> show (cpt) <> ") "  ) "" [1..6]
    where
        --------------------------------------
        cliques :: [Double]
        cliques = sort $ map (fromIntegral . size . _phyloClique_nodes) $ concat $ elems mFis
        -------------------------------------- 


traceSupport :: Map (Date, Date) [PhyloClique] -> String
traceSupport mFis = foldl (\msg cpt -> msg <> show (countSup cpt supports) <> " (>" <> show (cpt) <> ") "  ) "" [1..6]
    where
        --------------------------------------
        supports :: [Double]
        supports = sort $ map (fromIntegral . _phyloClique_support) $ concat $ elems mFis
        -------------------------------------- 


traceFis :: [Char] -> Map (Date, Date) [PhyloClique] -> Map (Date, Date) [PhyloClique]
traceFis msg mFis = trace ( "\n" <> "-- | " <> msg <> " : " <> show (sum $ map length $ elems mFis) <> "\n"
                         <> "Support : " <> (traceSupport mFis) <> "\n"
                         <> "Nb Ngrams : "  <> (traceClique mFis)  <> "\n" ) mFis


---------------
-- | Clique| --
---------------


getCliqueSupport :: Clique -> Int
getCliqueSupport unit = case unit of 
    Fis s _ -> s
    MaxClique _ -> 0

getCliqueSize :: Clique -> Int
getCliqueSize unit = case unit of 
    Fis _ s -> s
    MaxClique s -> s


--------------
-- | Cooc | --
--------------

listToCombi' :: [a] -> [(a,a)]
listToCombi' l = [(x,y) | (x:rest) <- tails l,  y <- rest]

listToEqual' :: Eq a => [a] -> [(a,a)]
listToEqual' l = [(x,y) | x <- l, y <- l, x == y]

listToKeys :: Eq a =>  [a] -> [(a,a)]
listToKeys lst = (listToCombi' lst) ++ (listToEqual' lst)

listToMatrix :: [Int] -> Map (Int,Int) Double
listToMatrix lst = fromList $ map (\k -> (k,1)) $ listToKeys $ sort lst

listToSeq :: Eq a =>  [a] -> [(a,a)]
listToSeq l = nubBy (\x y -> fst x == fst y) $ [ (x,y) | (x:rest) <- tails l,  y <- rest ]

sumCooc :: Cooc -> Cooc -> Cooc
sumCooc cooc cooc' = unionWith (+) cooc cooc'

getTrace :: Cooc -> Double 
getTrace cooc = sum $ elems $ filterWithKey (\(k,k') _ -> k == k') cooc

coocToDiago :: Cooc -> Cooc
coocToDiago cooc = filterWithKey (\(k,k') _ -> k == k') cooc

-- | To build the local cooc matrix of each phylogroup
ngramsToCooc :: [Int] -> [Cooc] -> Cooc
ngramsToCooc ngrams coocs =
    let cooc  = foldl (\acc cooc' -> sumCooc acc cooc') empty coocs
        pairs = listToKeys ngrams
    in  filterWithKey (\k _ -> elem k pairs) cooc


--------------------
-- | PhyloGroup | --
--------------------

getGroupId :: PhyloGroup -> PhyloGroupId 
getGroupId group = ((group ^. phylo_groupPeriod, group ^. phylo_groupLevel), group ^. phylo_groupIndex)

idToPrd :: PhyloGroupId -> PhyloPeriodId
idToPrd id = (fst . fst) id

getGroupThr :: PhyloGroup -> Double
getGroupThr group = last' "getGroupThr" ((group ^. phylo_groupMeta) ! "breaks")

groupByField :: Ord a => (PhyloGroup -> a) -> [PhyloGroup] ->  Map a [PhyloGroup]
groupByField toField groups = fromListWith (++) $ map (\g -> (toField g, [g])) groups

getPeriodPointers :: Filiation -> PhyloGroup -> [Pointer]
getPeriodPointers fil group = 
    case fil of 
        ToChilds  -> group ^. phylo_groupPeriodChilds
        ToParents -> group ^. phylo_groupPeriodParents

filterProximity :: Proximity -> Double -> Double -> Bool
filterProximity proximity thr local = 
    case proximity of
        WeightedLogJaccard _ -> local >= thr
        Hamming -> undefined   

getProximityName :: Proximity -> String
getProximityName proximity =
    case proximity of
        WeightedLogJaccard _ -> "WLJaccard"
        Hamming -> "Hamming"            

---------------
-- | Phylo | --
---------------

addPointers :: Filiation -> PointerType -> [Pointer] -> PhyloGroup -> PhyloGroup
addPointers fil pty pointers group = 
    case pty of 
        TemporalPointer -> case fil of 
                                ToChilds  -> group & phylo_groupPeriodChilds  .~ pointers
                                ToParents -> group & phylo_groupPeriodParents .~ pointers
        LevelPointer    -> case fil of 
                                ToChilds  -> group & phylo_groupLevelChilds   .~ pointers
                                ToParents -> group & phylo_groupLevelParents  .~ pointers


getPeriodIds :: Phylo -> [(Date,Date)]
getPeriodIds phylo = sortOn fst
                   $ keys
                   $ phylo ^. phylo_periods

getLevelParentId :: PhyloGroup -> PhyloGroupId 
getLevelParentId g = fst $ head' "getLevelParentId" $ g ^. phylo_groupLevelParents

getLastLevel :: Phylo -> Level
getLastLevel phylo = last' "lastLevel" $ getLevels phylo

getLevels :: Phylo -> [Level]
getLevels phylo = nub 
                $ map snd
                $ keys $ view ( phylo_periods
                       .  traverse
                       . phylo_periodLevels ) phylo

getSeaElevation :: Phylo -> SeaElevation
getSeaElevation phylo = seaElevation (getConfig phylo)


getConfig :: Phylo -> Config
getConfig phylo = (phylo ^. phylo_param) ^. phyloParam_config


getRoots :: Phylo -> Vector Ngrams
getRoots phylo = (phylo ^. phylo_foundations) ^. foundations_roots

phyloToLastBranches :: Phylo -> [[PhyloGroup]]
phyloToLastBranches phylo = elems 
    $ fromListWith (++)
    $ map (\g -> (g ^. phylo_groupBranchId, [g]))
    $ getGroupsFromLevel (last' "byBranches" $ getLevels phylo) phylo

getGroupsFromLevel :: Level -> Phylo -> [PhyloGroup]
getGroupsFromLevel lvl phylo = 
    elems $ view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse
                 .  filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl)
                 . phylo_levelGroups ) phylo


getGroupsFromLevelPeriods :: Level -> [PhyloPeriodId] -> Phylo -> [PhyloGroup]
getGroupsFromLevelPeriods lvl periods phylo = 
    elems $ view ( phylo_periods
                 .  traverse
                 .  filtered (\phyloPrd -> elem (phyloPrd ^. phylo_periodPeriod) periods)
                 . phylo_periodLevels
                 .  traverse
                 .  filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl)
                 . phylo_levelGroups ) phylo    


getGroupsFromPeriods :: Level -> Map PhyloPeriodId PhyloPeriod -> [PhyloGroup]
getGroupsFromPeriods lvl periods = 
    elems $ view (  traverse
                 . phylo_periodLevels
                 .  traverse
                 .  filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl)
                 . phylo_levelGroups ) periods


updatePhyloGroups :: Level -> Map PhyloGroupId PhyloGroup -> Phylo -> Phylo
updatePhyloGroups lvl m phylo = 
    over ( phylo_periods
         .  traverse
         . phylo_periodLevels
         .  traverse
         .  filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl)
         . phylo_levelGroups
         .  traverse 
         ) (\group -> 
                let id = getGroupId group
                in 
                    if member id m 
                    then m ! id
                    else group ) phylo


traceToPhylo :: Level -> Phylo -> Phylo
traceToPhylo lvl phylo = 
    trace ("\n" <> "-- | End of phylo making at level " <> show (lvl) <> " with "
                <> show (length $ getGroupsFromLevel lvl phylo) <> " groups and "
                <> show (length $ nub $ map _phylo_groupBranchId $ getGroupsFromLevel lvl phylo) <> " branches" <> "\n") phylo 

--------------------
-- | Clustering | --
--------------------

relatedComponents :: Ord a => [[a]] -> [[a]]
relatedComponents graph = foldl' (\acc groups ->
    if (null acc)
    then acc ++ [groups]
    else 
        let acc' = partition (\groups' -> disjoint (Set.fromList groups') (Set.fromList groups)) acc
         in (fst acc') ++ [nub $ concat $ (snd acc') ++ [groups]]) [] graph


traceSynchronyEnd :: Phylo -> Phylo
traceSynchronyEnd phylo = 
    trace ( "\n" <> "-- | End synchronic clustering at level " <> show (getLastLevel phylo) 
                 <> " with " <> show (length $ getGroupsFromLevel (getLastLevel phylo) phylo) <> " groups"
                 <> " and "  <> show (length $ nub $ map _phylo_groupBranchId $ getGroupsFromLevel (getLastLevel phylo) phylo) <> " branches"
                 <> "\n" ) phylo

traceSynchronyStart :: Phylo -> Phylo
traceSynchronyStart phylo = 
    trace ( "\n" <> "-- | Start synchronic clustering at level " <> show (getLastLevel phylo) 
                 <> " with " <> show (length $ getGroupsFromLevel (getLastLevel phylo) phylo) <> " groups"
                 <> " and "  <> show (length $ nub $ map _phylo_groupBranchId $ getGroupsFromLevel (getLastLevel phylo) phylo) <> " branches"
                 <> "\n" ) phylo    


-------------------
-- | Proximity | --
-------------------

getSensibility :: Proximity -> Double
getSensibility proxi = case proxi of 
    WeightedLogJaccard s -> s
    Hamming -> undefined

----------------
-- | Branch | --
----------------

intersectInit :: Eq a => [a] -> [a] -> [a] -> [a]
intersectInit acc lst lst' =
    if (null lst) || (null lst')
    then acc
    else if (head' "intersectInit" lst) == (head' "intersectInit" lst')
         then intersectInit (acc ++ [head' "intersectInit" lst]) (tail lst) (tail lst')
         else acc

branchIdsToProximity :: PhyloBranchId -> PhyloBranchId -> Double -> Double -> Double
branchIdsToProximity id id' thrInit thrStep = thrInit + thrStep * (fromIntegral $ length $ intersectInit [] (snd id) (snd id'))

ngramsInBranches :: [[PhyloGroup]] -> [Int]
ngramsInBranches branches = nub $ foldl (\acc g -> acc ++ (g ^. phylo_groupNgrams)) [] $ concat branches


traceMatchSuccess :: Double -> Double -> Double -> [[[PhyloGroup]]] -> [[[PhyloGroup]]]
traceMatchSuccess thr qua qua' nextBranches =
    trace ( "\n" <> "-- local branches : " <> (init $ show ((init . init . snd)
                                                    $ (head' "trace" $ head' "trace" $ head' "trace" nextBranches) ^. phylo_groupBranchId))
                                           <> ",(1.." <> show (length nextBranches) <> ")]"
                 <> " | " <> show ((length . concat . concat) nextBranches) <> " groups" <> "\n"
         <> " - splited with success in "  <> show (map length nextBranches) <> " sub-branches" <> "\n"
         <> " - for the local threshold "  <> show (thr) <> " ( quality : " <> show (qua) <> " < " <> show(qua') <> ")\n" ) nextBranches


traceMatchFailure :: Double -> Double -> Double -> [[PhyloGroup]] -> [[PhyloGroup]]
traceMatchFailure thr qua qua' branches =
    trace ( "\n" <> "-- local branches : " <> (init $ show ((init . snd) $ (head' "trace" $ head' "trace" branches) ^. phylo_groupBranchId))
                                           <> ",(1.." <> show (length branches) <> ")]"
                 <> " | " <> show (length $ concat branches) <> " groups" <> "\n"
         <> " - split with failure for the local threshold " <> show (thr) <> " ( quality : " <> show (qua) <> " > " <> show(qua') <> ")\n"
        ) branches


traceMatchNoSplit :: [[PhyloGroup]] -> [[PhyloGroup]]
traceMatchNoSplit branches =
    trace ( "\n" <> "-- local branches : " <> (init $ show ((init . snd) $ (head' "trace" $ head' "trace" branches) ^. phylo_groupBranchId))
                                           <> ",(1.." <> show (length branches) <> ")]"
                 <> " | " <> show (length $ concat branches) <> " groups" <> "\n"
         <> " - unable to split in smaller branches" <> "\n"
        ) branches


traceMatchLimit :: [[PhyloGroup]] -> [[PhyloGroup]]
traceMatchLimit branches =
    trace ( "\n" <> "-- local branches : " <> (init $ show ((init . snd) $ (head' "trace" $ head' "trace" branches) ^. phylo_groupBranchId))
                                           <> ",(1.." <> show (length branches) <> ")]"
                 <> " | " <> show (length $ concat branches) <> " groups" <> "\n"
         <> " - unable to increase the threshold above 1" <> "\n"
        ) branches            


traceMatchEnd :: [PhyloGroup] -> [PhyloGroup]
traceMatchEnd groups =
    trace ("\n" <> "-- | End temporal matching with " <> show (length $ nub $ map (\g -> g ^. phylo_groupBranchId) groups)
                                                         <> " branches and " <> show (length groups) <> " groups" <> "\n") groups


traceTemporalMatching :: [PhyloGroup] -> [PhyloGroup]
traceTemporalMatching groups = 
    trace ( "\n" <> "-- | Start temporal matching for " <> show(length groups) <> " groups" <> "\n") groups


traceGroupsProxi :: Map (PhyloGroupId,PhyloGroupId) Double -> Map (PhyloGroupId,PhyloGroupId) Double
traceGroupsProxi m = 
    trace ( "\n" <> "-- | " <> show(Map.size m) <> " computed pairs of groups proximity" <> "\n") m