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

import Control.Lens         hiding (both)
import Data.List            (filter, intersect, (++), sort)
import Data.Map             (Map)
import Data.Text            (Text)
import Data.Tuple.Extra
import Data.Vector          (Vector,elemIndex)
import Gargantext.Prelude   hiding (head)
import Gargantext.Viz.Phylo
import qualified Data.List  as List
import qualified Data.Map   as Map

------------------------------------------------------------------------
-- | Generic Tools | --

-- | To get the index of an element of a Vector
getIdx :: Eq a => a -> Vector a -> Int
getIdx x v = case (elemIndex x v) of
              Nothing -> panic "[ERR][Viz.Phylo.Tools.getIndex] Nothing"
              Just i  -> i

------------------------------------------------------------------------
-- | Phylomemic Tools | --

-- | To create a PhyloGroup in a Phylo out of a list of Ngrams and a set of parameters 
initGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> Phylo -> PhyloGroup
initGroup ngrams lbl idx lvl from to p = PhyloGroup 
  (((from, to), lvl), idx)
  lbl
  (sort $ map (\x -> ngramsToIdx x p) ngrams)
  (Map.empty)
  [] [] [] []

-- | To transform an Ngrams into its corresponding index in a Phylo 
ngramsToIdx :: Ngrams -> Phylo -> Int
ngramsToIdx x p = getIdx x (_phylo_ngrams p)

-- | To get the Ngrams of a PhyloGroup
getNgrams :: PhyloGroup -> [Int]
getNgrams =  _phylo_groupNgrams

-- | To get the id of a PhyloGroup
getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = _phylo_groupId

-- | To get all the PhyloGroup of a Phylo
getGroups :: Phylo -> [PhyloGroup]
getGroups = view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse 
                 . phylo_levelGroups
                 )

-- | To get the level out of the id of a PhyloGroup
getGroupLevel :: PhyloGroup -> Int
getGroupLevel = snd . fst . getGroupId

-- | To get the period out of the id of a PhyloGroup
getGroupPeriod :: PhyloGroup -> (Date,Date)
getGroupPeriod = fst . fst . getGroupId

-- | To filter the PhyloGroup of a Phylo according to a function and a value
filterGroups :: Eq a => (PhyloGroup -> a) -> a -> Phylo -> [PhyloGroup]
filterGroups f x p = filter (\g -> (f g) == x) (getGroups p)

-- | To get all the PhyloGroup of a Phylo with a given level and period
getGroupsWithFilters :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithFilters lvl prd p = (filterGroups getGroupLevel lvl p)
                                 `intersect`
                                 (filterGroups getGroupPeriod prd p)

-- | To alter each PhyloPeriod of a Phylo following a given function
alterPhyloPeriods :: (PhyloPeriod -> PhyloPeriod) -> Phylo -> Phylo
alterPhyloPeriods f p = over ( phylo_periods
                          .  traverse) f p

-- | To append a list of PhyloPeriod to a Phylo
appendPhyloPeriods :: [PhyloPeriod] -> Phylo -> Phylo
appendPhyloPeriods l p = over (phylo_periods) (++ l) p


