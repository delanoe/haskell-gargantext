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
import Data.List (sort, concat, null, union, (++), tails, sortOn, nub, init, tail)
import Data.Set (Set, size)
import Data.Map (Map, elems, fromList, unionWith, keys, member, (!), filterWithKey)
import Data.String (String)
import Data.Text (Text, unwords)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo

import Debug.Trace (trace)
import Control.Lens hiding (Level)

import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set

--------------
-- | Misc | --
--------------


countSup :: Double -> [Double] -> Int
countSup s l = length $ filter (>s) l


elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' e l = case (List.elemIndex e l) of
    Nothing -> panic ("[ERR][Viz.Phylo.PhyloTools] element not in list")
    Just i  -> i


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


traceClique :: Map (Date, Date) [PhyloFis] -> String
traceClique mFis = foldl (\msg cpt -> msg <> show (countSup cpt cliques) <> " (>" <> show (cpt) <> ") "  ) "" [1..6]
    where
        --------------------------------------
        cliques :: [Double]
        cliques = sort $ map (fromIntegral . size . _phyloFis_clique) $ concat $ elems mFis
        -------------------------------------- 


traceSupport :: Map (Date, Date) [PhyloFis] -> String
traceSupport mFis = foldl (\msg cpt -> msg <> show (countSup cpt supports) <> " (>" <> show (cpt) <> ") "  ) "" [1..6]
    where
        --------------------------------------
        supports :: [Double]
        supports = sort $ map (fromIntegral . _phyloFis_support) $ concat $ elems mFis
        -------------------------------------- 


traceFis :: [Char] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
traceFis msg mFis = trace ( "\n" <> "-- | " <> msg <> " : " <> show (sum $ map length $ elems mFis) <> "\n"
                         <> "Support : " <> (traceSupport mFis) <> "\n"
                         <> "Clique : "  <> (traceClique mFis)  <> "\n" ) mFis


-------------------------
-- | Contextual unit | --
-------------------------


getFisSupport :: ContextualUnit -> Int
getFisSupport unit = case unit of 
    Fis s _ -> s
    -- _       -> panic ("[ERR][Viz.Phylo.PhyloTools.getFisSupport] Only Fis has a support")

getFisSize :: ContextualUnit -> Int
getFisSize unit = case unit of 
    Fis _ s -> s
    -- _       -> panic ("[ERR][Viz.Phylo.PhyloTools.getFisSupport] Only Fis has a clique size")  


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

sumCooc :: Cooc -> Cooc -> Cooc
sumCooc cooc cooc' = unionWith (+) cooc cooc'

getTrace :: Cooc -> Double 
getTrace cooc = sum $ elems $ filterWithKey (\(k,k') _ -> k == k') cooc

--------------------
-- | PhyloGroup | --
--------------------

getGroupId :: PhyloGroup -> PhyloGroupId 
getGroupId group = ((group ^. phylo_groupPeriod, group ^. phylo_groupLevel), group ^. phylo_groupIndex)

---------------
-- | Phylo | --
---------------

addPointers :: PhyloGroup -> Filiation -> PointerType -> [Pointer] -> PhyloGroup
addPointers group fil pty pointers = 
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


getConfig :: Phylo -> Config
getConfig phylo = (phylo ^. phylo_param) ^. phyloParam_config


getRoots :: Phylo -> Vector Ngrams
getRoots phylo = (phylo ^. phylo_foundations) ^. foundations_roots


getGroupsFromLevel :: Level -> Phylo -> [PhyloGroup]
getGroupsFromLevel lvl phylo = 
    elems $ view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse
                 .  filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl)
                 . phylo_levelGroups ) phylo


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


-------------------
-- | Proximity | --
-------------------

getSensibility :: Proximity -> Double
getSensibility proxi = case proxi of 
    WeightedLogJaccard s _ _ -> s
    Hamming -> undefined

getThresholdInit :: Proximity -> Double
getThresholdInit proxi = case proxi of 
    WeightedLogJaccard _ t _ -> t
    Hamming -> undefined  

getThresholdStep :: Proximity -> Double
getThresholdStep proxi = case proxi of 
    WeightedLogJaccard _ _ s -> s
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
    trace ("\n" <> "-- | End of temporal matching with " <> show (length $ nub $ map (\g -> g ^. phylo_groupBranchId) groups)
                                                         <> " branches and " <> show (length groups) <> " groups" <> "\n") groups