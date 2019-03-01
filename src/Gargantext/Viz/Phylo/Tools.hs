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
import Data.List            (filter, intersect, (++), sort, null, head, tail, last)
import Data.Map             (Map)
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
filterGroups :: Eq a => (PhyloGroup -> a) -> a -> Phylo -> [PhyloGroup]
filterGroups f x p = filter (\g -> (f g) == x) (getGroups p)


-- | To filter nested Sets of a
filterNestedSets :: Eq a => Set a -> [Set a] -> [Set a] -> [Set a]
filterNestedSets h l l'
  | null l                 = if doesAnySetContains h l l'
                             then l'
                             else h : l'
  | doesAnySetContains h l l' = filterNestedSets (head l) (tail l) l'
  | otherwise              = filterNestedSets (head l) (tail l) (h : l')


-- | To get the id of a PhyloGroup
getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = _phylo_groupId


-- | To get the level out of the id of a PhyloGroup
getGroupLevel :: PhyloGroup -> Int
getGroupLevel = snd . fst . getGroupId


-- | To get the Ngrams of a PhyloGroup
getGroupNgrams :: PhyloGroup -> [Int]
getGroupNgrams =  _phylo_groupNgrams


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


-- | To get all the PhyloGroup of a Phylo with a given level and period
getGroupsWithFilters :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithFilters lvl prd p = (filterGroups getGroupLevel lvl p)
                                 `intersect`
                                 (filterGroups getGroupPeriod prd p)


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


-- | To get all the Phylolevels of a given PhyloPeriod
getPhyloLevels :: PhyloPeriod -> [PhyloLevel]
getPhyloLevels = view (phylo_periodLevels)


-- | To get the Ngrams of a Phylo
getPhyloNgrams :: Phylo -> PhyloNgrams
getPhyloNgrams = _phylo_ngrams


-- | To create a PhyloGroup in a Phylo out of a list of Ngrams and a set of parameters 
initGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> Phylo -> PhyloGroup
initGroup ngrams lbl idx lvl from to p = PhyloGroup 
  (((from, to), lvl), idx)
  lbl
  (sort $ map (\x -> ngramsToIdx x p) ngrams)
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