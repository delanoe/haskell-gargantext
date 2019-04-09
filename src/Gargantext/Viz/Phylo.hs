{-|
Module      : Gargantext.Viz.Phylo
Description : Phylomemy definitions and types.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Specifications of Phylomemy export format.

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
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.Phylo where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON,defaultOptions)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Set     (Set)
import Data.Map     (Map)
import Data.Vector  (Vector)
--import Data.Time.Clock.POSIX  (POSIXTime)
import GHC.Generics (Generic)
--import Gargantext.Database.Schema.Ngrams (NgramsId)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

--------------------
-- | PhyloParam | --
--------------------


-- | Global parameters of a Phylo
data PhyloParam =
     PhyloParam { _phyloParam_version  :: Text -- Double ?
                , _phyloParam_software :: Software
                , _phyloParam_query    :: PhyloQueryBuild
     } deriving (Generic, Show, Eq)


-- | Software parameters
data Software =
     Software { _software_name    :: Text
              , _software_version :: Text
     } deriving (Generic, Show, Eq)


---------------
-- | Phylo | --
---------------


-- | Phylo datatype of a phylomemy
-- Duration    : time Segment of the whole Phylo
-- Foundations : vector of all the Ngrams contained in a Phylo (build from a list of actants)
-- Periods     : list of all the periods of a Phylo
data Phylo =
     Phylo { _phylo_duration    :: (Start, End)
           , _phylo_foundations :: Vector Ngrams
           , _phylo_foundationsPeaks :: PhyloPeaks
           , _phylo_periods     :: [PhyloPeriod]
           , _phylo_param       :: PhyloParam
           }
           deriving (Generic, Show, Eq)

-- | The PhyloPeaks describe the aggregation of some foundations Ngrams behind a list of Ngrams trees (ie: a forest)
-- PeaksLabels are the root labels of each Ngrams trees
data PhyloPeaks =
      PhyloPeaks { _phylo_peaksLabels :: Vector Ngrams
                 , _phylo_peaksForest :: [Tree Ngrams]
                 }
                 deriving (Generic, Show, Eq)

-- | A Tree of Ngrams where each node is a label
data Tree a = Empty | Node a [Tree a] deriving (Show, Eq)


-- | Date : a simple Integer
type Date = Int

-- | UTCTime in seconds since UNIX epoch
-- type Start   = POSIXTime
-- type End     = POSIXTime
type Start   = Date
type End     = Date


---------------------
-- | PhyloPeriod | --
---------------------


-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloPeriod =
     PhyloPeriod { _phylo_periodId     :: PhyloPeriodId
                 , _phylo_periodLevels :: [PhyloLevel]
                 }
                 deriving (Generic, Show, Eq)


--------------------
-- | PhyloLevel | --
--------------------


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
                deriving (Generic, Show, Eq)


--------------------
-- | PhyloGroup | --
--------------------


-- | PhyloGroup : group of ngrams at each level and step
-- Label : maybe has a label as text
-- Ngrams: set of terms that build the group
-- Quality : map of measures (support, etc.) that depict some qualitative aspects of a phylo
-- Period Parents|Childs: weighted link to Parents|Childs (Temporal Period   axis)
-- Level  Parents|Childs: weighted link to Parents|Childs (Level Granularity axis)
-- Pointers are directed link from Self to any PhyloGroup (/= Self ?)
data PhyloGroup =
     PhyloGroup { _phylo_groupId            :: PhyloGroupId
                , _phylo_groupLabel         :: Text
                , _phylo_groupNgrams        :: [Int]
                , _phylo_groupMeta          :: Map Text Double
                , _phylo_groupCooc          :: Map (Int, Int) Double
                , _phylo_groupBranchId      :: Maybe PhyloBranchId

                , _phylo_groupPeriodParents :: [Pointer]
                , _phylo_groupPeriodChilds  :: [Pointer]

                , _phylo_groupLevelParents  :: [Pointer]
                , _phylo_groupLevelChilds   :: [Pointer]
                }
                deriving (Generic, Show, Eq, Ord)


-- | Level : A level of aggregation (-1 = Txt, 0 = Ngrams, 1 = Fis, [2..] = Cluster)
type Level = Int
-- | Index : A generic index of an element (PhyloGroup, PhyloBranch, etc) in a given List
type Index = Int


type PhyloPeriodId = (Start, End)
type PhyloLevelId  = (PhyloPeriodId, Level)
type PhyloGroupId  = (PhyloLevelId, Index)
type PhyloBranchId = (Level, Index)


-- | Weight : A generic mesure that can be associated with an Id
type Weight = Double
-- | Pointer : A weighted linked with a given PhyloGroup
type Pointer = (PhyloGroupId, Weight)
-- | Ngrams : a contiguous sequence of n terms
type Ngrams = Text


--------------------
-- | Aggregates | --
--------------------


-- | Document : a piece of Text linked to a Date
data Document = Document
      { date :: Date
      , text :: [Ngrams]
      } deriving (Show)

-- | Clique : Set of ngrams cooccurring in the same Document
type Clique   = Set Ngrams
-- | Support : Number of Documents where a Clique occurs
type Support  = Int
-- | Fis : Frequent Items Set (ie: the association between a Clique and a Support)
data PhyloFis = PhyloFis
  { _phyloFis_clique  :: Clique
  , _phyloFis_support :: Support
  , _phyloFis_metrics :: Map (Int,Int) (Map Text [Double])
  } deriving (Show)

-- | A list of clustered PhyloGroup
type PhyloCluster = [PhyloGroup]


-- | A List of PhyloGroup in a Graph
type GroupNodes = [PhyloGroup]
-- | A List of weighted links between some PhyloGroups in a Graph
type GroupEdges = [((PhyloGroup,PhyloGroup),Weight)]
-- | The association as a Graph between a list of Nodes and a list of Edges
type GroupGraph = (GroupNodes,GroupEdges)


---------------
-- | Error | --
---------------


data PhyloError = LevelDoesNotExist
                | LevelUnassigned
          deriving (Show)


-----------------
-- | Cluster | --
-----------------


-- | Cluster constructors
data Cluster = Fis FisParams
             | RelatedComponents RCParams
             | Louvain LouvainParams
        deriving (Generic, Show, Eq)

-- | Parameters for Fis clustering
data FisParams = FisParams
  { _fis_keepMinorFis :: Bool
  , _fis_minSupport   :: Support
  } deriving (Generic, Show, Eq)

-- | Parameters for RelatedComponents clustering
data RCParams = RCParams
  { _rc_proximity :: Proximity } deriving (Generic, Show, Eq)

-- | Parameters for Louvain clustering
data LouvainParams = LouvainParams
  { _louvain_proximity :: Proximity } deriving (Generic, Show, Eq)


-------------------
-- | Proximity | --
-------------------


-- | Proximity constructors
data Proximity = WeightedLogJaccard WLJParams
               | Hamming HammingParams
               | Filiation
          deriving (Generic, Show, Eq)

-- | Parameters for WeightedLogJaccard proximity
data WLJParams = WLJParams
  { _wlj_threshold   :: Double
  , _wlj_sensibility :: Double
  } deriving (Generic, Show, Eq)

-- | Parameters for Hamming proximity
data HammingParams = HammingParams
  { _hamming_threshold :: Double } deriving (Generic, Show, Eq)


----------------
-- | Filter | --
----------------


-- | Filter constructors
data Filter = SmallBranch SBParams deriving (Generic, Show, Eq)

-- | Parameters for SmallBranch filter
data SBParams = SBParams
  { _sb_periodsInf :: Int
  , _sb_periodsSup :: Int
  , _sb_minNodes   :: Int } deriving (Generic, Show, Eq)


----------------
-- | Metric | --
----------------


-- | Metric constructors
data Metric = BranchAge deriving (Generic, Show, Eq)


----------------
-- | Tagger | --
----------------


-- | Tagger constructors
data Tagger = BranchLabelFreq | GroupLabelCooc | GroupDynamics deriving (Show)


--------------
-- | Sort | --
--------------


-- | Sort constructors
data Sort  = ByBranchAge deriving (Generic, Show)
data Order = Asc | Desc  deriving (Generic, Show)


--------------------
-- | PhyloQuery | --
--------------------


-- | A Phyloquery describes a phylomemic reconstruction
data PhyloQueryBuild = PhyloQueryBuild
    { _q_phyloTitle :: Text
    , _q_phyloDesc  :: Text

    -- Grain and Steps for the PhyloPeriods
    , _q_periodGrain :: Int
    , _q_periodSteps :: Int

    -- Clustering method for building the contextual unit of Phylo (ie: level 1)
    , _q_contextualUnit :: Cluster
    , _q_contextualUnitMetrics :: [Metric]
    , _q_contextualUnitFilters :: [Filter]

    -- Inter-temporal matching method of the Phylo
    , _q_interTemporalMatching :: Proximity

    -- Last level of reconstruction
    , _q_nthLevel   :: Level
    -- Clustering method used from level 1 to nthLevel
    , _q_nthCluster :: Cluster
    } deriving (Generic, Show, Eq)

-- | To choose the Phylo edge you want to export : --> <-- <--> <=>
data Filiation = Ascendant | Descendant | Merge | Complete deriving (Generic, Show)
data EdgeType  = PeriodEdge | LevelEdge deriving (Generic, Show)

-------------------
-- | PhyloView | --
-------------------


-- | A PhyloView is the output type of a Phylo
data PhyloView = PhyloView
  { _pv_param       :: PhyloParam
  , _pv_title       :: Text
  , _pv_description :: Text
  , _pv_filiation   :: Filiation
  , _pv_metrics     :: Map Text [Double]
  , _pv_branches    :: [PhyloBranch]
  , _pv_nodes       :: [PhyloNode]
  , _pv_edges       :: [PhyloEdge]
  } deriving (Generic, Show)

-- | A phyloview is made of PhyloBranches, edges and nodes
data PhyloBranch = PhyloBranch
  { _pb_id      :: PhyloBranchId
  , _pb_label   :: Text
  , _pb_metrics :: Map Text [Double]
  } deriving (Generic, Show)

data PhyloEdge = PhyloEdge
  { _pe_source :: PhyloGroupId
  , _pe_target :: PhyloGroupId
  , _pe_type   :: EdgeType
  , _pe_weight :: Weight
  } deriving (Generic, Show)

data PhyloNode = PhyloNode
  { _pn_id        :: PhyloGroupId
  , _pn_bid  :: Maybe PhyloBranchId
  , _pn_label     :: Text
  , _pn_idx :: [Int]
  , _pn_ngrams    :: Maybe [Ngrams]
  , _pn_metrics      :: Map Text [Double]
  , _pn_parents :: Maybe [PhyloGroupId]
  , _pn_childs  :: [PhyloNode]
  } deriving (Generic, Show)

------------------------
-- | PhyloQueryView | --
------------------------


data DisplayMode = Flat | Nested

-- | A PhyloQueryView describes a Phylo as an output view
data PhyloQueryView = PhyloQueryView
  { _qv_lvl    :: Level

  -- Does the PhyloGraph contain ascendant, descendant or a complete Filiation ? Complet redondant et merge (avec le max)
  , _qv_filiation :: Filiation

  -- Does the PhyloGraph contain some levelChilds ? How deep must it go ?
  , _qv_levelChilds      :: Bool
  , _qv_levelChildsDepth :: Level

  -- Ordered lists of filters, taggers and metrics to be applied to the PhyloGraph
  -- Firstly the metrics, then the filters and the taggers
  , _qv_metrics :: [Metric]
  , _qv_filters :: [Filter]
  , _qv_taggers :: [Tagger]

  -- An asc or desc sort to apply to the PhyloGraph
  , _qv_sort :: Maybe (Sort,Order)

  -- A display mode to apply to the PhyloGraph, ie: [Node[Node,Edge],Edge] or [[Node,Node],[Edge,Edge]]
  , _qv_display :: DisplayMode
  , _qv_verbose :: Bool
  }


----------------
-- | Lenses | --
----------------


makeLenses ''PhyloParam
makeLenses ''Software
--
makeLenses ''Phylo
makeLenses ''PhyloPeaks
makeLenses ''PhyloGroup
makeLenses ''PhyloLevel
makeLenses ''PhyloPeriod
makeLenses ''PhyloFis
--
makeLenses ''Proximity
makeLenses ''Cluster
makeLenses ''Filter
--
makeLenses ''PhyloQueryBuild
makeLenses ''PhyloQueryView
--
makeLenses ''PhyloView
makeLenses ''PhyloBranch
makeLenses ''PhyloNode
makeLenses ''PhyloEdge


------------------------
-- | JSON instances | --
------------------------


$(deriveJSON (unPrefix "_phylo_"       ) ''Phylo       )
$(deriveJSON (unPrefix "_phylo_peaks"  ) ''PhyloPeaks  )
$(deriveJSON defaultOptions ''Tree  )
$(deriveJSON (unPrefix "_phylo_period" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phylo_level"  ) ''PhyloLevel  )
$(deriveJSON (unPrefix "_phylo_group"  ) ''PhyloGroup  )
$(deriveJSON (unPrefix "_phyloFis_"    ) ''PhyloFis    )
--
$(deriveJSON (unPrefix "_software_"    ) ''Software    )
$(deriveJSON (unPrefix "_phyloParam_"  ) ''PhyloParam  )
--
$(deriveJSON defaultOptions ''Filter    )
$(deriveJSON defaultOptions ''Metric    )
$(deriveJSON defaultOptions ''Cluster   )
$(deriveJSON defaultOptions ''Proximity )
--
$(deriveJSON (unPrefix "_fis_" )     ''FisParams     )
$(deriveJSON (unPrefix "_hamming_" ) ''HammingParams )
$(deriveJSON (unPrefix "_louvain_" ) ''LouvainParams )
$(deriveJSON (unPrefix "_rc_" )      ''RCParams      )
$(deriveJSON (unPrefix "_wlj_" )     ''WLJParams     )
$(deriveJSON (unPrefix "_sb_" )      ''SBParams      )
--
$(deriveJSON (unPrefix "_q_" )  ''PhyloQueryBuild  )
$(deriveJSON (unPrefix "_pv_" ) ''PhyloView   )
$(deriveJSON (unPrefix "_pb_" ) ''PhyloBranch )
$(deriveJSON (unPrefix "_pe_" ) ''PhyloEdge   )
$(deriveJSON (unPrefix "_pn_" ) ''PhyloNode   )

$(deriveJSON defaultOptions ''Filiation )
$(deriveJSON defaultOptions ''EdgeType  )


----------------------------
-- | TODO XML instances | --
----------------------------

