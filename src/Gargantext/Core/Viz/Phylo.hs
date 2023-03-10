{-
Module      : Gargantext.Core.Viz.AdaptativePhylo
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

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Viz.Phylo where

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import Data.Swagger
import Data.Text   (Text, pack)
import Data.Vector (Vector)
import GHC.Generics
import GHC.IO (FilePath)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Prelude
import qualified Data.Text.Lazy as TextLazy

----------------
-- | PhyloConfig | --
----------------

data CorpusParser =
      Wos  {_wos_limit  :: Int}
    | Csv  {_csv_limit  :: Int}
    | Csv' {_csv'_limit :: Int}
    deriving (Show,Generic,Eq)

instance ToSchema CorpusParser where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")


data ListParser = V3 | V4 deriving (Show,Generic,Eq)
instance ToSchema ListParser


data SeaElevation =
      Constante
      { _cons_start :: Double
      , _cons_gap   :: Double }
    | Adaptative
      { _adap_steps :: Double }
    | Evolving
      { _evol_neighborhood :: Bool }      
    deriving (Show,Generic,Eq)

instance ToSchema SeaElevation

data PhyloSimilarity =
      WeightedLogJaccard
      { _wlj_sensibility     :: Double
      , _wlj_minSharedNgrams :: Int }
    | WeightedLogSim
      { _wls_sensibility     :: Double
      , _wls_minSharedNgrams :: Int }
    | Hamming 
      { _hmg_sensibility     :: Double 
      , _hmg_minSharedNgrams :: Int}

    deriving (Show,Generic,Eq)

instance ToSchema PhyloSimilarity where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data SynchronyScope = SingleBranch | SiblingBranches | AllBranches
  deriving (Show,Generic,Eq, ToSchema)

data SynchronyStrategy = MergeRegularGroups | MergeAllGroups
  deriving (Show,Generic,Eq)

instance ToSchema SynchronyStrategy where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data Synchrony =
      ByProximityThreshold
      { _bpt_threshold :: Double
      , _bpt_sensibility :: Double
      , _bpt_scope :: SynchronyScope
      , _bpt_strategy :: SynchronyStrategy }
    | ByProximityDistribution
      { _bpd_sensibility :: Double
      , _bpd_strategy :: SynchronyStrategy }
    deriving (Show,Generic,Eq)

instance ToSchema Synchrony where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")



data TimeUnit =
      Epoch
      { _epoch_period :: Int
      , _epoch_step   :: Int
      , _epoch_matchingFrame :: Int }
    | Year
      { _year_period :: Int
      , _year_step   :: Int
      , _year_matchingFrame :: Int }
    | Month
      { _month_period :: Int
      , _month_step   :: Int
      , _month_matchingFrame :: Int }
    | Week
      { _week_period :: Int
      , _week_step   :: Int
      , _week_matchingFrame :: Int }
    | Day
      { _day_period :: Int
      , _day_step   :: Int
      , _day_matchingFrame :: Int }
      deriving (Show,Generic,Eq)

instance ToSchema TimeUnit where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data MaxCliqueFilter = ByThreshold | ByNeighbours deriving (Show,Generic,Eq)

instance ToSchema MaxCliqueFilter where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")



data Cluster =
      Fis
      { _fis_support :: Int
      , _fis_size    :: Int }
    | MaxClique
      { _mcl_size      :: Int
      , _mcl_threshold :: Double
      , _mcl_filter    :: MaxCliqueFilter }
      deriving (Show,Generic,Eq)

instance ToSchema Cluster where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data Quality =
     Quality { _qua_granularity :: Double
             , _qua_minBranch   :: Int }
      deriving (Show,Generic,Eq)

instance ToSchema Quality where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_qua_")


data PhyloConfig =
     PhyloConfig { corpusPath     :: FilePath
            , listPath       :: FilePath
            , outputPath     :: FilePath
            , corpusParser   :: CorpusParser
            , listParser     :: ListParser
            , phyloName      :: Text
            , phyloScale     :: Int
            , similarity     :: PhyloSimilarity
            , seaElevation   :: SeaElevation
            , defaultMode    :: Bool
            , findAncestors  :: Bool
            , phyloSynchrony :: Synchrony
            , phyloQuality   :: Quality
            , timeUnit       :: TimeUnit
            , clique         :: Cluster
            , exportLabel    :: [PhyloLabel]
            , exportSort     :: Sort
            , exportFilter   :: [Filter]
            } deriving (Show,Generic,Eq)


------------------------------------------------------------------------
data PhyloSubConfig =
  PhyloSubConfig { _sc_phyloProximity :: Double
                 , _sc_phyloSynchrony :: Double
                 , _sc_phyloQuality   :: Double
                 , _sc_timeUnit       :: TimeUnit
                 , _sc_clique         :: Cluster
                 , _sc_exportFilter   :: Double
                 }
  deriving (Show,Generic,Eq)


subConfig2config :: PhyloSubConfig -> PhyloConfig
subConfig2config subConfig = defaultConfig { similarity     = WeightedLogJaccard (_sc_phyloProximity subConfig) 1 
                                           , phyloSynchrony = ByProximityThreshold (_sc_phyloSynchrony subConfig) 0 AllBranches MergeAllGroups
                                           , phyloQuality   = Quality (_sc_phyloQuality   subConfig) 1
                                           , timeUnit       = _sc_timeUnit       subConfig
                                           , clique         = _sc_clique         subConfig
                                           , exportFilter   = [ByBranchSize $ _sc_exportFilter   subConfig]
                                           }

------------------------------------------------------------------------
defaultConfig :: PhyloConfig
defaultConfig =
     PhyloConfig { corpusPath = "corpus.csv" -- useful for commandline only
            , listPath       = "list.csv"   -- useful for commandline only
            , outputPath     = "data/"
            , corpusParser   = Csv 100000
            , listParser     = V4
            , phyloName      = pack "Phylo Name"
            , phyloScale     = 2
            , similarity     = WeightedLogJaccard 0.5 1
            , seaElevation   = Constante 0.1 0.1
            , defaultMode    = True
            , findAncestors  = False
            , phyloSynchrony = ByProximityThreshold 0.5 0 AllBranches MergeAllGroups
            , phyloQuality   = Quality 0.5 1
            , timeUnit       = Year 3 1 5
            , clique         = MaxClique 5 0.0001 ByThreshold
            , exportLabel    = [BranchLabel MostEmergentTfIdf 2, GroupLabel MostEmergentInclusive 2]
            , exportSort     = ByHierarchy Desc
            , exportFilter   = [ByBranchSize 3]
            }

-- Main Instances
instance ToSchema PhyloConfig
instance ToSchema PhyloSubConfig

instance FromJSON PhyloConfig
instance ToJSON PhyloConfig

instance FromJSON PhyloSubConfig
instance ToJSON PhyloSubConfig

instance FromJSON CorpusParser
instance ToJSON CorpusParser

instance FromJSON ListParser
instance ToJSON ListParser

instance FromJSON PhyloSimilarity
instance ToJSON PhyloSimilarity

instance FromJSON SeaElevation
instance ToJSON SeaElevation

instance FromJSON TimeUnit
instance ToJSON TimeUnit

instance FromJSON MaxCliqueFilter
instance ToJSON MaxCliqueFilter

instance FromJSON Cluster
instance ToJSON Cluster

instance FromJSON PhyloLabel
instance ToJSON PhyloLabel

instance FromJSON Tagger
instance ToJSON Tagger

instance FromJSON Sort
instance ToJSON Sort

instance FromJSON Order
instance ToJSON Order

instance FromJSON Filter
instance ToJSON Filter

instance FromJSON SynchronyScope
instance ToJSON SynchronyScope

instance FromJSON SynchronyStrategy
instance ToJSON SynchronyStrategy

instance FromJSON Synchrony
instance ToJSON Synchrony

instance FromJSON Quality
instance ToJSON Quality


-- | Software parameters
data Software =
     Software { _software_name    :: Text
              , _software_version :: Text
     } deriving (Generic, Show, Eq)

instance ToSchema Software where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_software_")



defaultSoftware :: Software
defaultSoftware =
      Software { _software_name    = pack "GarganText"
               , _software_version = pack "v5" }


-- | Global parameters of a Phylo
data PhyloParam =
     PhyloParam { _phyloParam_version  :: Text
                , _phyloParam_software :: Software
                , _phyloParam_config   :: PhyloConfig
     } deriving (Generic, Show, Eq)

instance ToSchema PhyloParam where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phyloParam_")



defaultPhyloParam :: PhyloParam
defaultPhyloParam =
      PhyloParam { _phyloParam_version  = pack "v3"
                 , _phyloParam_software = defaultSoftware
                 , _phyloParam_config   = defaultConfig }


------------------
-- | Document | --
------------------

-- | Date : a simple Integer
type Date = Int

-- | DateStr : the string version of a Date
type DateStr = Text

-- | Ngrams : a contiguous sequence of n terms
type Ngrams = Text

-- Document : a piece of Text linked to a Date
-- date = computational date; date' = original string date yyyy-mm-dd
-- Export Database to Document
data Document = Document
      { date    :: Date   -- datatype Date {unDate :: Int}
      , date'   :: DateStr   -- show date
      , text    :: [Ngrams]
      , weight  :: Maybe Double
      , sources :: [Text]
      } deriving (Eq,Show,Generic,NFData)


--------------------
-- | Foundation | --
--------------------


-- | The Foundations of a Phylo created from a given TermList
data PhyloFoundations = PhyloFoundations
      { _foundations_roots    :: (Vector Ngrams)
      , _foundations_rootsInGroups :: Map Int [PhyloGroupId] -- map of roots associated to groups
      } deriving (Generic, Show, Eq)

data PhyloCounts = PhyloCounts
      { coocByDate    :: !(Map Date Cooc)
      , docsByDate    :: !(Map Date Double)
      , rootsCount    :: !(Map Int  Double)
      , rootsFreq     :: !(Map Int  Double)
      , lastRootsFreq :: !(Map Int  Double)
      } deriving (Generic, Show, Eq)

data PhyloSources = PhyloSources
      { _sources :: !(Vector Text) } deriving (Generic, Show, Eq)

instance ToSchema PhyloFoundations where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_foundations_")
instance ToSchema PhyloCounts where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")
instance ToSchema PhyloSources where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")

---------------------------
-- | Coocurency Matrix | --
---------------------------


-- | Cooc : a coocurency matrix between two ngrams
type Cooc =  Map (Int,Int) Double


-------------------
-- | Phylomemy | --
-------------------

-- | Period : a tuple of Dates
type Period = (Date,Date)

-- | PeriodStr : a tuple of DateStr
type PeriodStr = (DateStr,DateStr)




-- | Phylo datatype of a phylomemy
--  foundations : the foundations of the phylo
--  timeCooc : a Map of coocurency by minimal unit of time (ex: by year)
--  timeDocs : a Map with the numbers of docs by minimal unit of time (ex: by year)
--  param : the parameters of the phylomemy (with the user's configuration)
--  periods : the temporal steps of a phylomemy
data Phylo =
     Phylo { _phylo_foundations  :: PhyloFoundations
           , _phylo_sources      :: PhyloSources
           , _phylo_counts       :: PhyloCounts
           , _phylo_seaLadder    :: [Double]
           , _phylo_param        :: PhyloParam
           , _phylo_periods      :: Map Period PhyloPeriod
           , _phylo_quality      :: Double
           , _phylo_level        :: Double
           }
           deriving (Generic, Show, Eq)

instance ToSchema Phylo where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_")


----------------
-- | Period | --
----------------

-- | PhyloPeriod : steps of a phylomemy on a temporal axis
--  id: tuple (start date, end date) of the temporal step of the phylomemy
--  scales: scales of synchronic description
data PhyloPeriod =
     PhyloPeriod { _phylo_periodPeriod    :: Period
                 , _phylo_periodPeriodStr :: PeriodStr
                 , _phylo_periodScales    :: Map PhyloScaleId PhyloScale
                 } deriving (Generic, Show, Eq)

instance ToSchema PhyloPeriod where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_")

---------------
-- | Scale | --
---------------

-- | Scale : a scale of synchronic description
type Scale = Int

-- | PhyloScaleId : the id of a scale of synchronic description
type PhyloScaleId  = (Period,Scale)

-- | PhyloScale : sub-structure of the phylomemy in scale of synchronic description
data PhyloScale =
     PhyloScale { _phylo_scalePeriod    :: Period
                , _phylo_scalePeriodStr :: PeriodStr
                , _phylo_scaleScale     :: Scale
                , _phylo_scaleGroups    :: Map PhyloGroupId PhyloGroup
                }
                deriving (Generic, Show, Eq)

instance ToSchema PhyloScale where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_")


type PhyloGroupId  = (PhyloScaleId, Int)

-- | BranchId : (a scale, a sequence of branch index)
-- the sequence is a path of heritage from the most to the less specific branch
type PhyloBranchId = (Scale, [Int])

-- | PhyloGroup : group of ngrams at each scale and period
data PhyloGroup =
      PhyloGroup { _phylo_groupPeriod   :: Period
                 , _phylo_groupPeriod'  :: (Text,Text)
                 , _phylo_groupScale    :: Scale
                 , _phylo_groupIndex    :: Int
                 , _phylo_groupLabel    :: Text
                 , _phylo_groupSupport  :: Support
                 , _phylo_groupWeight   :: Maybe Double
                 , _phylo_groupSources  :: [Int]
                 , _phylo_groupNgrams   :: [Int]
                 , _phylo_groupCooc     :: !(Cooc)
                 , _phylo_groupBranchId :: PhyloBranchId
                 , _phylo_groupMeta     :: Map Text [Double]
                 , _phylo_groupScaleParents  :: [Pointer]
                 , _phylo_groupScaleChilds   :: [Pointer]
                 , _phylo_groupPeriodParents :: [Pointer]
                 , _phylo_groupPeriodChilds  :: [Pointer]
                 , _phylo_groupAncestors     :: [Pointer]
                 , _phylo_groupPeriodMemoryParents :: [Pointer']
                 , _phylo_groupPeriodMemoryChilds  :: [Pointer']
                 }
                 deriving (Generic, Show, Eq, NFData)

instance ToSchema PhyloGroup where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_")


-- | Weight : A generic mesure that can be associated with an Id
type Weight = Double
type Thr = Double

-- | Pointer : A weighted pointer to a given PhyloGroup
type Pointer = (PhyloGroupId, Weight)
-- | Pointer' : A weighted pointer to a given PhyloGroup with a lower bounded threshold
type Pointer' = (PhyloGroupId, (Thr,Weight))

data Filiation = ToParents | ToChilds | ToParentsMemory | ToChildsMemory deriving (Generic, Show)
data PointerType = TemporalPointer | ScalePointer deriving (Generic, Show)


--------------------------
-- | Phylo Clustering | --
--------------------------

-- | Support : Number of Documents where a Cluster occurs
type Support  = Int

data Clustering = Clustering
  { _clustering_roots   :: [Int]
  , _clustering_support :: Support
  , _clustering_period  :: Period
  -- additional materials for visualization
  , _clustering_visWeighting :: Maybe Double
  , _clustering_visFiltering :: [Int]
  } deriving (Generic,NFData,Show,Eq)

----------------
-- | Export | --
----------------

type DotId = TextLazy.Text

data EdgeType = GroupToGroup | GroupToGroupMemory | BranchToGroup | BranchToBranch | GroupToAncestor | PeriodToPeriod deriving (Show,Generic,Eq)

data Filter = ByBranchSize { _branch_size :: Double } deriving (Show,Generic,Eq)
instance ToSchema Filter where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data Order = Asc | Desc deriving (Show,Generic,Eq, ToSchema)

data Sort = ByBirthDate { _sort_order :: Order } | ByHierarchy {_sort_order :: Order } deriving (Show,Generic,Eq)
instance ToSchema Sort where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_sort_")


data Tagger = MostInclusive | MostEmergentInclusive | MostEmergentTfIdf deriving (Show,Generic,Eq)
instance ToSchema Tagger where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


data PhyloLabel =
      BranchLabel
      { _branch_labelTagger :: Tagger
      , _branch_labelSize   :: Int }
    | GroupLabel
      { _group_labelTagger  :: Tagger
      , _group_labelSize    :: Int }
    deriving (Show,Generic,Eq)

instance ToSchema PhyloLabel where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")


data PhyloBranch =
      PhyloBranch
      { _branch_id :: PhyloBranchId
      , _branch_canonId  :: [Int]
      , _branch_seaLevel :: [Double]
      , _branch_x        :: Double
      , _branch_y        :: Double
      , _branch_w        :: Double
      , _branch_t        :: Double
      , _branch_label    :: Text
      , _branch_meta     :: Map Text [Double]
      } deriving (Generic, Show, Eq)

instance ToSchema PhyloBranch where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_branch_")

data PhyloExport =
      PhyloExport
      { _export_groups    :: [PhyloGroup]
      , _export_branches  :: [PhyloBranch]
      } deriving (Generic, Show)
instance ToSchema PhyloExport where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_export_")


----------------
-- | Lenses | --
----------------

makeLenses ''PhyloConfig
makeLenses ''PhyloSubConfig
makeLenses ''PhyloSimilarity
makeLenses ''SeaElevation
makeLenses ''Quality
makeLenses ''Cluster
makeLenses ''PhyloLabel
makeLenses ''TimeUnit
makeLenses ''PhyloFoundations
makeLenses ''Clustering
makeLenses ''Phylo
makeLenses ''PhyloPeriod
makeLenses ''PhyloScale
makeLenses ''PhyloGroup
makeLenses ''PhyloParam
makeLenses ''PhyloExport
makeLenses ''PhyloBranch

------------------------
-- | JSON instances | --
------------------------

instance FromJSON Phylo
instance ToJSON Phylo

instance FromJSON PhyloSources
instance ToJSON PhyloSources

instance FromJSON PhyloParam
instance ToJSON PhyloParam

instance FromJSON PhyloCounts
instance ToJSON PhyloCounts

instance FromJSON PhyloPeriod
instance ToJSON PhyloPeriod

instance FromJSON PhyloScale
instance ToJSON PhyloScale

instance FromJSON Software
instance ToJSON Software

instance FromJSON PhyloGroup
instance ToJSON PhyloGroup

$(deriveJSON (unPrefix "_foundations_"  ) ''PhyloFoundations)
