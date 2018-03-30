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
-- Period : time Segment of the whole phylomemy in UTCTime format (start,end)
-- Ngrams : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps  : list of all steps to build the phylomemy
data Phylo = Phylo { _phyloPeriod :: (Start, End)
                   , _phyloNgrams :: [Ngram]
                   , _phyloSteps  :: [PhyloStep]
                   } deriving (Generic)

type Start   = UTCTime
type End     = UTCTime

type Ngram   = (NgramId, Text)
type NgramId = Int

-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloStep = PhyloStep { _phyloStepPeriod :: (Start, End)
                           , _phyloStepLevels :: [PhyloLevel]
                           } deriving (Generic)

-- | PhyloLevel : levels of phylomemy on level axis
-- Levels description:
-- Level 0: Ngram equals itself              (by identity) == _phyloNgrams
-- Level 1: Group gathers synonyms           (by stems + by qualitative expert meaning)
-- Level 2: Group is Frequent Item Set       (by statistics)
-- Level 3: Group is a cluster or community  (by statistics)

type PhyloLevel = [PhyloGroup]

-- | PhyloGroup : group of ngrams at each level and step
-- Label: maybe has a label as text
-- Ngrams: set of terms that build the group
-- Temporal Parents|Childs: directed and weighted link to Parents|Childs (Temporal axis)
-- Granularity Parents|Childs: directed and weighted link to Parents|Childs (Granularity axis)
data PhyloGroup = PhyloGroup { _phyloGroupLabel              :: Maybe Text
                             , _phyloGroupNgrams             :: [NgramId]
                   
                             , _phyloGroupTemporalParents    :: [Edge]
                             , _phyloGroupTemporalChilds     :: [Edge]
                   
                             , _phyloGroupGranularityParents :: [Edge]
                             , _phyloGroupGranularityChilds  :: [Edge]
                             } deriving (Generic)

type Edge   = (NgramId, Weight)
type Weight = Double

-- | JSON instances
$(deriveJSON (unPrefix "_phylo") ''Phylo)
$(deriveJSON (unPrefix "_phyloStep")  ''PhyloStep)
$(deriveJSON (unPrefix "_phyloGroup") ''PhyloGroup)
