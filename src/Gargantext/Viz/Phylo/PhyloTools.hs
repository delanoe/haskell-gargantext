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
import Data.List (sort, concat, null, union, (++), tails, sortOn)
import Data.Set (Set, size)
import Data.Map (Map, elems, fromList, unionWith, keys, member, (!), toList)
import Data.String (String)

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
                                ToChilds  -> group & phylo_groupPeriodChilds  %~ (++ pointers)
                                ToParents -> group & phylo_groupPeriodParents %~ (++ pointers)
        LevelPointer    -> case fil of 
                                ToChilds  -> group & phylo_groupLevelChilds %~ (++ pointers)
                                ToParents -> group & phylo_groupLevelParents %~ (++ pointers)


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


------------------
-- | Pointers | --
------------------

pointersToLinks :: PhyloGroupId -> [Pointer] -> [Link]
pointersToLinks id pointers = map (\p -> ((id,fst p),snd p)) pointers

mergeLinks :: [Link] -> [Link] -> [Link]
mergeLinks toChilds toParents = 
    let toChilds' = fromList $ map (\((from,to),w) -> ((to,from),w)) toChilds
    in  toList $ unionWith max (fromList toParents) toChilds' 