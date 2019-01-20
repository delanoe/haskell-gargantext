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

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Time.Clock.POSIX  (POSIXTime)
import GHC.Generics (Generic)
import Gargantext.Database.Schema.Ngrams (NgramsId)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

------------------------------------------------------------------------
data PhyloFormat =
     PhyloFormat { _phyloFormat_parm :: PhyloParam
                 , _phyloFormat_data :: Phylo
     } deriving (Generic)

-- | .phylo parameters
data PhyloParam = 
     PhyloParam { _phyloParam_version     :: Text -- Double ?
                , _phyloParam_software    :: Software
                , _phyloParam_params      :: Hash
     } deriving (Generic)

type Hash = Text

-- | Software
-- TODO move somewhere since it is generic
data Software =
     Software { _software_name    :: Text
              , _software_version :: Text
     } deriving (Generic)

------------------------------------------------------------------------
-- | Phylo datatype descriptor of a phylomemy
-- Duration : time Segment of the whole phylomemy (start,end)
-- Ngrams   : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps    : list of all steps to build the phylomemy
data Phylo =
     Phylo { _phylo_puration :: (Start, End)
           , _phylo_ngrams   :: [Ngram]
           , _phylo_periods  :: [PhyloPeriod]
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
     PhyloPeriod { _phylo_periodId     :: PhyloPeriodId
                 , _phylo_periodLevels :: [PhyloLevel]
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
     PhyloLevel { _phylo_levelId     :: PhyloLevelId
                , _phylo_levelGroups :: [PhyloGroup]
                }
                deriving (Generic)

type PhyloLevelId = (PhyloPeriodId, Int)

-- | PhyloGroup : group of ngrams at each level and step
-- Label : maybe has a label as text
-- Ngrams: set of terms that build the group
-- Period Parents|Childs: weighted link to Parents|Childs (Temporal Period   axis)
-- Level  Parents|Childs: weighted link to Parents|Childs (Level Granularity axis)
data PhyloGroup =
     PhyloGroup { _phylo_groupId    :: PhyloGroupId
                , _phylo_groupLabel :: Maybe Text
                , _phylo_groupNgrams        :: [NgramsId]
                
                , _phylo_groupPeriodParents :: [Edge]
                , _phylo_groupPeriodChilds  :: [Edge]
                
                , _phylo_groupLevelParents  :: [Edge]
                , _phylo_groupLevelChilds   :: [Edge]
                }
                deriving (Generic)

type PhyloGroupId = (PhyloLevelId, Int)
type Edge         = (PhyloGroupId, Weight)
type Weight       = Double

-- | Lenses
makeLenses ''Phylo
makeLenses ''PhyloParam
makeLenses ''PhyloFormat
makeLenses ''Software

-- | JSON instances
$(deriveJSON (unPrefix "_phylo_"       ) ''Phylo       )
$(deriveJSON (unPrefix "_phylo_period" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phylo_level"  ) ''PhyloLevel  )
$(deriveJSON (unPrefix "_phylo_group"  ) ''PhyloGroup  )
-- 
$(deriveJSON (unPrefix "_software_"  ) ''Software )
$(deriveJSON (unPrefix "_phyloParam_"  ) ''PhyloParam  )
$(deriveJSON (unPrefix "_phyloFormat_" ) ''PhyloFormat  )

-- | TODO XML instances

