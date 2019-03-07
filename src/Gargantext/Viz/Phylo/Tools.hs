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

module Gargantext.Viz.Phylo.Tools
  where

import Control.Lens         hiding (both, Level)
import Data.List            (filter, intersect, (++), sort, null, head, tail, last, tails)
import Data.Map             (Map, mapKeys, member)
import Data.Set             (Set)
import Data.Text            (Text)
import Data.Tuple.Extra
import Data.Vector          (Vector,elemIndex)
import Gargantext.Prelude   hiding (head)
import Gargantext.Viz.Phylo

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set


------------------------------------------------------------------------
-- | Tools | --


-- | To add a new PhyloGroupId to a PhyloBranch
addGroupIdToBranch :: PhyloGroupId -> PhyloBranch -> PhyloBranch
addGroupIdToBranch id b = over (phylo_branchGroups) (++ [id]) b


-- | To add a PhyloLevel at the end of a list of PhyloLevels
addPhyloLevel :: PhyloLevel -> [PhyloLevel] -> [PhyloLevel]
addPhyloLevel lvl l = l ++ [lvl] 


-- | To alter each list of PhyloGroups following a given function
alterPhyloGroups :: ([PhyloGroup] -> [PhyloGroup]) -> Phylo -> Phylo
alterPhyloGroups f p = over ( phylo_periods
                            .  traverse
                            . phylo_periodLevels
                            .  traverse
                            . phylo_levelGroups
                            ) f p 


-- | To alter each PhyloPeriod of a Phylo following a given function
alterPhyloPeriods :: (PhyloPeriod -> PhyloPeriod) -> Phylo -> Phylo
alterPhyloPeriods f p = over ( phylo_periods
                             .  traverse) f p


-- | To alter the list of PhyloBranches of a Phylo
alterPhyloBranches :: ([PhyloBranch] -> [PhyloBranch]) -> Phylo -> Phylo
alterPhyloBranches f p = over ( phylo_branches ) f p


-- | To alter a list of PhyloLevels following a given function
alterPhyloLevels :: ([PhyloLevel] -> [PhyloLevel]) -> Phylo -> Phylo
alterPhyloLevels f p = over ( phylo_periods
                            .  traverse
                            . phylo_periodLevels) f p


-- | To append a list of PhyloPeriod to a Phylo
appendPhyloPeriods :: [PhyloPeriod] -> Phylo -> Phylo
appendPhyloPeriods l p = over (phylo_periods) (++ l) p


-- | Does a List of Sets contains at least one Set of an other List
doesAnySetContains :: Eq a =>  Set a -> [Set a] -> [Set a] -> Bool
doesAnySetContains h l l' = any (\c -> doesContains (Set.toList c) (Set.toList h)) (l' ++ l)


-- | Does a list of A contains an other list of A
doesContains :: Eq a => [a] -> [a] -> Bool
doesContains l l'
  | null l'               = True
  | length l' > length l  = False
  | elem (head l') l      = doesContains l (tail l')
  | otherwise             = False


-- | Does a list of ordered A contains an other list of ordered A
doesContainsOrd :: Eq a => Ord a => [a] -> [a] -> Bool
doesContainsOrd l l'
  | null l'          = False
  | last l < head l' = False
  | head l' `elem` l = True
  | otherwise        = doesContainsOrd l (tail l')


 -- | To filter the PhyloGroup of a Phylo according to a function and a value
filterGroups :: Eq a => (PhyloGroup -> a) -> a -> [PhyloGroup] -> [PhyloGroup]
filterGroups f x l = filter (\g -> (f g) == x) l


-- | To filter nested Sets of a
filterNestedSets :: Eq a => Set a -> [Set a] -> [Set a] -> [Set a]
filterNestedSets h l l'
  | null l                 = if doesAnySetContains h l l'
                             then l'
                             else h : l'
  | doesAnySetContains h l l' = filterNestedSets (head l) (tail l) l'
  | otherwise              = filterNestedSets (head l) (tail l) (h : l')


-- | To get the PhyloGroups Childs of a PhyloGroup
getGroupChilds :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupChilds g p = getGroupsFromIds (map fst $ _phylo_groupPeriodChilds g) p


-- | To get the id of a PhyloGroup
getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = _phylo_groupId


-- | To get the Cooc Matrix of a PhyloGroup
getGroupCooc :: PhyloGroup -> Map (Int,Int) Double
getGroupCooc = _phylo_groupCooc


-- | To get the level out of the id of a PhyloGroup
getGroupLevel :: PhyloGroup -> Int
getGroupLevel = snd . fst . getGroupId


-- | To get the Ngrams of a PhyloGroup
getGroupNgrams :: PhyloGroup -> [Int]
getGroupNgrams =  _phylo_groupNgrams


-- | To get the list of pairs (Childs & Parents) of a PhyloGroup
getGroupPairs :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupPairs g p = (getGroupChilds g p) ++ (getGroupParents g p)


-- | To get the PhyloGroups Parents of a PhyloGroup
getGroupParents :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupParents g p = getGroupsFromIds (map fst $ _phylo_groupPeriodParents g) p


-- | To get the period out of the id of a PhyloGroup
getGroupPeriod :: PhyloGroup -> (Date,Date)
getGroupPeriod = fst . fst . getGroupId


-- | To get all the PhyloGroup of a Phylo
getGroups :: Phylo -> [PhyloGroup]
getGroups = view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse 
                 . phylo_levelGroups
                 )


-- | To all PhyloGroups matching a list of PhyloGroupIds in a Phylo
getGroupsFromIds :: [PhyloGroupId] -> Phylo -> [PhyloGroup]
getGroupsFromIds ids p = filter (\g -> elem (getGroupId g) ids) $ getGroups p


-- | To get all the PhyloGroup of a Phylo with a given level and period
getGroupsWithFilters :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithFilters lvl prd p = (getGroupsWithLevel  lvl p)
                                 `intersect`
                                 (getGroupsWithPeriod prd p)


-- | To get all the PhyloGroup of a Phylo with a given Level
getGroupsWithLevel :: Int -> Phylo -> [PhyloGroup]
getGroupsWithLevel lvl p = filterGroups getGroupLevel lvl (getGroups p)


-- | To get all the PhyloGroup of a Phylo with a given Period
getGroupsWithPeriod :: (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithPeriod prd p = filterGroups getGroupPeriod prd (getGroups p)


-- | To get the index of an element of a Vector
getIdx :: Eq a => a -> Vector a -> Int
getIdx x v = case (elemIndex x v) of
              Nothing -> panic "[ERR][Viz.Phylo.Tools.getIndex] Nothing"
              Just i  -> i


-- | To get the label of a Level
getLevelLabel :: Level -> LevelLabel
getLevelLabel lvl = _levelLabel lvl


-- | To get the value of a Level
getLevelValue :: Level -> Int
getLevelValue lvl = _levelValue lvl


-- | To get the label of a LevelLink based on a Direction
getLevelLinkLabel :: Direction -> LevelLink -> LevelLabel
getLevelLinkLabel dir link = case dir of 
    From -> view (levelFrom . levelLabel) link
    To   -> view (levelTo   . levelLabel) link 
    _    -> panic "[ERR][Viz.Phylo.Tools.getLevelLinkLabel] Wrong direction"


-- | To get the value of a LevelLink based on a Direction
getLevelLinkValue :: Direction -> LevelLink -> Int
getLevelLinkValue dir link = case dir of 
    From -> view (levelFrom . levelValue) link
    To   -> view (levelTo   . levelValue) link 
    _    -> panic "[ERR][Viz.Phylo.Tools.getLevelLinkValue] Wrong direction"


-- | To get the Branches of a Phylo
getPhyloBranches :: Phylo -> [PhyloBranch] 
getPhyloBranches = _phylo_branches


-- | To get all the Phylolevels of a given PhyloPeriod
getPhyloLevels :: PhyloPeriod -> [PhyloLevel]
getPhyloLevels = view (phylo_periodLevels)


-- | To get the Ngrams of a Phylo
getPhyloNgrams :: Phylo -> PhyloNgrams
getPhyloNgrams = _phylo_ngrams


-- | To get all the PhyloPeriodIds of a Phylo
getPhyloPeriods :: Phylo -> [PhyloPeriodId]
getPhyloPeriods p = map _phylo_periodId 
                  $ view (phylo_periods) p


-- | To create a PhyloGroup in a Phylo out of a list of Ngrams and a set of parameters 
initGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> Phylo -> PhyloGroup
initGroup ngrams lbl idx lvl from to p = PhyloGroup 
  (((from, to), lvl), idx)
  lbl
  (sort $ map (\x -> ngramsToIdx x p) ngrams)
  (Map.empty)
  (Map.empty)
  [] [] [] []


-- | To create a Level
initLevel :: Int -> LevelLabel -> Level
initLevel lvl lbl = Level lbl lvl


-- | To create a LevelLink
initLevelLink :: Level -> Level -> LevelLink 
initLevelLink lvl lvl' = LevelLink lvl lvl' 


-- | To create a PhyloLevel
initPhyloLevel :: PhyloLevelId -> [PhyloGroup] -> PhyloLevel
initPhyloLevel id groups = PhyloLevel id groups


-- | To create a PhyloPeriod
initPhyloPeriod :: PhyloPeriodId -> [PhyloLevel] -> PhyloPeriod
initPhyloPeriod id l = PhyloPeriod id l


-- | To get all combinations of a list
listToDirectedCombi :: Eq a => [a] -> [(a,a)]
listToDirectedCombi l = [(x,y) | x <- l, y <- l, x /= y]


-- | To get all combinations of a list and apply a function to the resulting list of pairs
listToDirectedCombiWith :: Eq a => forall b. (a -> b) -> [a] -> [(b,b)]
listToDirectedCombiWith f l = [(f x,f y) | x <- l, y <- l, x /= y]


-- | To get all combinations of a list with no repetition
listToUnDirectedCombi :: [a] -> [(a,a)]
listToUnDirectedCombi l = [ (x,y) | (x:rest) <- tails l,  y <- rest ]


-- | To get all combinations of a list with no repetition and apply a function to the resulting list of pairs
listToUnDirectedCombiWith :: forall a b. (a -> b) -> [a] -> [(b,b)]
listToUnDirectedCombiWith f l = [ (f x, f y) | (x:rest) <- tails l,  y <- rest ]


-- | To transform an Ngrams into its corresponding index in a Phylo 
ngramsToIdx :: Ngrams -> Phylo -> Int
ngramsToIdx x p = getIdx x (_phylo_ngrams p)


-- | To set the LevelId of a PhyloLevel and of all its PhyloGroups
setPhyloLevelId :: Int -> PhyloLevel -> PhyloLevel
setPhyloLevelId lvl' (PhyloLevel (id, lvl) groups)
    = PhyloLevel (id, lvl') groups'
        where 
            groups' = over (traverse . phylo_groupId) (\((period, lvl), idx) -> ((period, lvl'), idx)) groups 


-- | To choose a LevelLink strategy based an a given Level 
shouldLink :: LevelLink -> [Int] -> [Int] -> Bool
shouldLink lvl l l'
  | from <= 1 = doesContainsOrd l l'
  | from > 1  = undefined
  | otherwise = panic ("[ERR][Viz.Phylo.Tools.shouldLink] LevelLink not defined") 
  where
    -------------------------------------- 
    from :: Int
    from = getLevelLinkValue From lvl
    --------------------------------------


-- | To unify the keys (x,y) that Map 1 share with Map 2 such as: (x,y) <=> (y,x)
unifySharedKeys :: Eq a => Ord a => Map (a,a) b -> Map (a,a) b -> Map (a,a) b
unifySharedKeys m1 m2 = mapKeys (\(x,y) -> if member (y,x) m2
                                           then (y,x)
                                           else (x,y) ) m1 