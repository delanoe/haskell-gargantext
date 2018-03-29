{-|
Module      : Gargantext.Types.Nodes
Description : Main Types of Nodes
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Specifications of Phylomemy format.

Phylomemy can be described as a Temporal Graph with different scale of
granularity of group of terms.

The main type is Phylo which is synonym of Phylomemy (only difference is
the number of chars).

Phylomemy was first described in [REF].
-}

{-# LANGUAGE DeriveGeneric        #-}

module Gargantext.Types.Phylo where

import Data.Aeson   (ToJSON, FromJSON)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Time    (UTCTime)

import GHC.Generics (Generic)

import Gargantext.Prelude

------------------------------------------------------------------------
-- | Phylo datatype descriptors:
-- Period: time Segment of the whole phylomemy in UTCTime format (start,end)
-- Terms : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps : list of all steps to build the phylomemy
data Phylo = Phylo { _phyloPeriod :: (Start, End)
                   , _phyloNgrams :: [Ngram]
                   , _phyloSteps  :: [PhyloStep]
                   } deriving (Generic)

type Ngram   = (NgramId, Text)
type NgramId = Int

type Start   = UTCTime
type End     = UTCTime

-- | PhyloStep data type descriptor
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloStep = PhyloStep { _phyloStepPeriod :: (Start, End)
                           , _phyloStepLevels :: [Level]
                           } deriving (Generic)

-- | Level of a step of a Phylomemy
-- Label: maybe has a label as text
-- Terms: set of terms that build the group
-- Temporal Parents: directed and weighted link to Parents
-- Levels description:
-- Level 0: Ngram equals itself      (by identity) == _phyloNgrams
-- Level 1: Semantic grouping        (by stems + by qualitative expert meaning)
-- Level 2: Frequent Item Set groups (by statistics)
-- Level 3: Clustering               (by statistics)
data Level = Level { _levelLabel              :: Maybe Text
                   , _levelTerms              :: [NgramId]
                   
                   , _levelTemporalParents    :: [NgramId]
                   , _levelTemporalChilds     :: [NgramId]
                   
                   , _levelGranularityParents :: [NgramId]
                   , _levelGranularityChilds  :: [NgramId]
              } deriving (Generic)

-- | JSON instances
instance FromJSON Phylo
instance ToJSON   Phylo

instance FromJSON PhyloStep
instance ToJSON   PhyloStep

instance FromJSON Level
instance ToJSON   Level

