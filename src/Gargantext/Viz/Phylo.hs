{-|
Module      : Gargantext.Viz.Phylo
Description : Phylomemy definitions and types.
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

References: 
Chavalarias, D., Cointet, J.-P., 2013. Phylomemetic patterns
in science evolution — the rise and fall of scientific fields. PloS
one 8, e54847.

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Viz.Phylo where

import Data.Aeson.TH (deriveJSON)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Time.Clock.POSIX  (POSIXTime)
import GHC.Generics (Generic)
import Gargantext.Database.Ngram (NgramsId)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

------------------------------------------------------------------------
-- | Phylo datatype descriptor of a phylomemy
-- Duration : time Segment of the whole phylomemy (start,end)
-- Ngrams   : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps    : list of all steps to build the phylomemy
data Phylo =
     Phylo { _phylo_Duration :: (Start, End)
           , _phylo_Ngrams   :: [Ngram]
           , _phylo_Periods  :: [PhyloPeriod]
           }
           deriving (Generic)

-- | UTCTime in seconds since UNIX epoch
type Start   = POSIXTime
type End     = POSIXTime

-- | Indexed Ngram
type Ngram   = (NgramsId, Text)

-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloPeriod =
     PhyloPeriod { _phylo_PeriodId     :: PhyloPeriodId
                 , _phylo_PeriodLevels :: [PhyloLevel]
                 } 
                 deriving (Generic)

type PhyloPeriodId = (Start, End)

-- | PhyloLevel : levels of phylomemy on level axis
-- Levels description:
-- Level -1: Ngram equals itself         (by identity) == _phylo_Ngrams
-- Level  0: Group of synonyms           (by stems + by qualitative expert meaning)
-- Level  1: First level of clustering
-- Level  N: Nth   level of clustering
data PhyloLevel =
     PhyloLevel { _phylo_LevelId     :: PhyloLevelId
                , _phylo_LevelGroups :: [PhyloGroup]
                }
                deriving (Generic)

type PhyloLevelId = (PhyloPeriodId, Int)

-- | PhyloGroup : group of ngrams at each level and step
-- Label : maybe has a label as text
-- Ngrams: set of terms that build the group
-- Period Parents|Childs: weighted link to Parents|Childs (Temporal Period   axis)
-- Level  Parents|Childs: weighted link to Parents|Childs (Level Granularity axis)
data PhyloGroup =
     PhyloGroup { _phylo_GroupId    :: PhyloGroupId
                , _phylo_GroupLabel :: Maybe Text
                , _phylo_GroupNgrams        :: [NgramsId]
                
                , _phylo_GroupPeriodParents :: [Edge]
                , _phylo_GroupPeriodChilds  :: [Edge]
                
                , _phylo_GroupLevelParents  :: [Edge]
                , _phylo_GroupLevelChilds   :: [Edge]
                }
                deriving (Generic)

type PhyloGroupId = (PhyloLevelId, Int)
type Edge         = (PhyloGroupId, Weight)
type Weight       = Double

-- | JSON instances
$(deriveJSON (unPrefix "_phylo_"       ) ''Phylo       )
$(deriveJSON (unPrefix "_phylo_Period" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phylo_Level"  ) ''PhyloLevel  )
$(deriveJSON (unPrefix "_phylo_Group"  ) ''PhyloGroup  )
