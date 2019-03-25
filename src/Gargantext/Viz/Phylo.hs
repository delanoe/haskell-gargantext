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
import Data.Aeson.TH (deriveJSON)
import Data.Maybe   (Maybe)
import Data.Text    (Text)
import Data.Set     (Set)
import Data.Map     (Map)
import Data.Vector  (Vector)
import Data.Time.Clock.POSIX  (POSIXTime)
import GHC.Generics (Generic)
import Gargantext.Database.Schema.Ngrams (NgramsId)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

------------------------------------------------------------------------
data PhyloExport =
     PhyloExport { _phyloExport_param :: PhyloParam
                 , _phyloExport_data :: Phylo
     } deriving (Generic, Show)

-- | .phylo parameters
data PhyloParam = 
     PhyloParam { _phyloParam_version     :: Text -- Double ?
                , _phyloParam_software    :: Software
                , _phyloParam_params      :: Hash
     } deriving (Generic, Show)

type Hash = Text

-- | Software
-- TODO move somewhere since it is generic
data Software =
     Software { _software_name    :: Text
              , _software_version :: Text
     } deriving (Generic, Show)

------------------------------------------------------------------------

-- | Phylo datatype of a phylomemy
-- Duration    : time Segment of the whole Phylo
-- Foundations : vector of all the Ngrams contained in a Phylo (build from a list of actants)
-- Periods     : list of all the periods of a Phylo
data Phylo =
     Phylo { _phylo_duration    :: (Start, End)
           , _phylo_foundations :: Vector Ngrams
           , _phylo_periods     :: [PhyloPeriod]
           }
           deriving (Generic, Show)


-- | Date : a simple Integer
type Date = Int

-- | UTCTime in seconds since UNIX epoch
-- type Start   = POSIXTime
-- type End     = POSIXTime
type Start   = Date
type End     = Date

-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloPeriod =
     PhyloPeriod { _phylo_periodId     :: PhyloPeriodId
                 , _phylo_periodLevels :: [PhyloLevel]
                 } 
                 deriving (Generic, Show)


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
                deriving (Generic, Show)


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


-- | Clique : Set of ngrams cooccurring in the same Document
type Clique  = Set Ngrams
-- | Support : Number of Documents where a Clique occurs
type Support = Int 
-- | Fis : Frequent Items Set (ie: the association between a Clique and a Support) 
type Fis = (Clique,Support)


-- | Document : a piece of Text linked to a Date
data Document = Document
      { date :: Date
      , text :: Text
      } deriving (Show)


type Cluster = [PhyloGroup]


-- | A List of PhyloGroup in a Graph
type GroupNodes = [PhyloGroup]
-- | A List of weighted links between some PhyloGroups in a Graph
type GroupEdges = [((PhyloGroup,PhyloGroup),Weight)]
-- | The association as a Graph between a list of Nodes and a list of Edges
type GroupGraph = (GroupNodes,GroupEdges)


data PhyloError = LevelDoesNotExist
                | LevelUnassigned
          deriving (Show)               


-- | A List of Proximity mesures or strategies 
data Proximity  = WeightedLogJaccard | Hamming | FromPairs
-- | A List of Clustering methods 
data Clustering  = Louvain | RelatedComponents

data PairTo = Childs | Parents 

------------------------------------------------------------------------
-- | To export a Phylo | --


-- | PhyloView | --


data Filiation = Ascendant | Descendant | Complete deriving (Show)
data EdgeType  = PeriodEdge | LevelEdge deriving (Show)

data PhyloView = PhyloView
  { _phylo_viewParam       :: PhyloParam
  , _phylo_viewLabel       :: Text
  , _phylo_viewDescription :: Text
  , _phylo_viewFiliation   :: Filiation
  , _phylo_viewMeta        :: Map Text Double
  , _phylo_viewBranches    :: [PhyloBranch]
  , _phylo_viewNodes       :: [PhyloNode]
  , _phylo_viewEdges       :: [PhyloEdge]
  } deriving (Show)


data PhyloBranch = PhyloBranch 
  { _phylo_branchId    :: PhyloBranchId
  , _phylo_branchLabel :: Text
  , _phylo_branchMeta  :: Map Text Double
  } deriving (Show)  


data PhyloEdge = PhyloEdge
  { _phylo_edgeSource :: PhyloGroupId
  , _phylo_edgeTarget :: PhyloGroupId
  , _phylo_edgeType   :: EdgeType
  , _phylo_edgeWeight :: Weight
  } deriving (Show)


data PhyloNode = PhyloNode
  { _phylo_nodeId        :: PhyloGroupId
  , _phylo_nodeBranchId  :: Maybe PhyloBranchId
  , _phylo_nodeLabel     :: Text
  , _phylo_nodeNgramsIdx :: [Int] 
  , _phylo_nodeNgrams    :: Maybe [Ngrams]
  , _phylo_nodeMeta      :: Map Text Double
  , _phylo_nodeParent    :: Maybe PhyloGroupId 
  , _phylo_nodeChilds    :: [PhyloNode]
  } deriving (Show)

-- | PhyloQuery | --


data Filter = LonelyBranch 
data Metric = BranchAge
data Tagger = BranchLabelFreq | GroupLabelCooc | GroupDynamics


data Sort   = ByBranchAge
data Order  = Asc | Desc 

data DisplayMode = Flat | Nested 


-- | A query filter seen as : prefix && ((filter params)(clause)) 
data QueryFilter = QueryFilter
  { _query_filter :: Filter
  , _query_params :: [Double]
  }


-- | A PhyloQuery is the structured representation of a user query to be applied to a Phylo
data PhyloQuery = PhyloQuery 
  { _query_lvl    :: Level

  -- Does the PhyloGraph contain ascendant, descendant or a complete Filiation ?
  , _query_filiation :: Filiation

  -- Does the PhyloGraph contain some levelChilds ? How deep must it go ?
  , _query_childs      :: Bool
  , _query_childsDepth :: Level

  -- Ordered lists of filters, taggers and metrics to be applied to the PhyloGraph
  -- Firstly the metrics, then the filters and the taggers   
  , _query_metrics :: [Metric]
  , _query_filters :: [QueryFilter]
  , _query_taggers :: [Tagger]

  -- An asc or desc sort to apply to the PhyloGraph
  , _query_sort :: Maybe (Sort,Order)

  -- A display mode to apply to the PhyloGraph, ie: [Node[Node,Edge],Edge] or [[Node,Node],[Edge,Edge]] 
  , _query_display :: DisplayMode
  , _query_verbose :: Bool
  }


------------------------------------------------------------------------
-- | Lenses and Json | --


-- | Lenses
makeLenses ''Phylo
makeLenses ''PhyloParam
makeLenses ''PhyloExport
makeLenses ''Software
makeLenses ''PhyloGroup
makeLenses ''PhyloLevel
makeLenses ''PhyloPeriod
makeLenses ''PhyloView
makeLenses ''PhyloQuery
makeLenses ''PhyloBranch
makeLenses ''PhyloNode
makeLenses ''PhyloEdge
makeLenses ''QueryFilter

-- | JSON instances
$(deriveJSON (unPrefix "_phylo_"       ) ''Phylo       ) 
$(deriveJSON (unPrefix "_phylo_period" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phylo_level"  ) ''PhyloLevel  )
$(deriveJSON (unPrefix "_phylo_group"  ) ''PhyloGroup  )
-- 
$(deriveJSON (unPrefix "_software_"    ) ''Software    )
$(deriveJSON (unPrefix "_phyloParam_"  ) ''PhyloParam  )
$(deriveJSON (unPrefix "_phyloExport_" ) ''PhyloExport )

-- | TODO XML instances

