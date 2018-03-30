{-|
Module      : Gargantext.Types.Phylo
Description : Main Types for Phylomemy
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Specifications of Phylomemy format.

Phylomemy can be described as a Temporal Graph with different scale of
granularity of group of ngrams (terms and multi-terms).

The main type is Phylo which is synonym of Phylomemy (only difference is
the number of chars).

Phylomemy was first described in [REF].
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Types.Phylo where

import Data.Aeson.TH (deriveJSON)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Time    (UTCTime)

import GHC.Generics (Generic)

import Gargantext.Prelude
import Gargantext.Utils.Prefix (unPrefix)

------------------------------------------------------------------------
-- | Phylo datatype descriptor of a phylomemy
-- Duration : time Segment of the whole phylomemy in UTCTime format (start,end)
-- Ngrams   : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps    : list of all steps to build the phylomemy
data Phylo = Phylo { _phyloDuration :: (Start, End)
                   , _phyloNgrams   :: [Ngram]
                   , _phyloPeriods    :: [PhyloPeriod]
                   } deriving (Generic)

type Start   = UTCTime
type End     = UTCTime

type Ngram   = (NgramId, Text)
type NgramId = Int

-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloPeriod = PhyloPeriod { _phyloPeriodDuration :: (Start, End)
                               , _phyloPeriodLevels :: [PhyloLevel]
                               } deriving (Generic)

-- | PhyloLevel : levels of phylomemy on level axis
-- Levels description:
-- Level -1: Ngram equals itself         (by identity) == _phyloNgrams
-- Level  0: Group of synonyms           (by stems + by qualitative expert meaning)
-- Level  1: First level of clustering
-- Level  N: Nth   level of clustering
type PhyloLevel = [PhyloGroup]

-- | PhyloGroup : group of ngrams at each level and step
-- Label : maybe has a label as text
-- Ngrams: set of terms that build the group
-- Period Parents|Childs: directed and weighted link to Parents|Childs (Temporal Period axis)
-- Level  Parents|Childs: directed and weighted link to Parents|Childs (Level Granularity axis)
data PhyloGroup = PhyloGroup { _phyloGroupId            :: GroupId
                             , _phyloGroupLabel         :: Maybe Text
                             , _phyloGroupNgrams        :: [NgramId]
                   
                             , _phyloGroupPeriodParents :: [Edge]
                             , _phyloGroupPeriodChilds  :: [Edge]
                   
                             , _phyloGroupLevelParents  :: [Edge]
                             , _phyloGroupLevelChilds   :: [Edge]
                             } deriving (Generic)

type Edge   = (PhyloGroupId, Weight)
type Weight       = Double
type PhyloGroupId = Int

-- | JSON instances
$(deriveJSON (unPrefix "_phylo"       ) ''Phylo       )
$(deriveJSON (unPrefix "_phyloPeriod" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phyloGroup"  ) ''PhyloGroup  )
