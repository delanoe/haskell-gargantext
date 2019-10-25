{-|
Module      : Gargantext.Viz.AdaptativePhylo
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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.AdaptativePhylo where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text   (Text, pack)
import Data.Vector (Vector)
import Data.Map (Map)
import Data.Set (Set)

import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude
import Gargantext.Text.Context (TermList)

import GHC.Generics
import GHC.IO (FilePath)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)

import qualified Data.Text.Lazy as TextLazy


----------------
-- | Config | --
----------------  


data CorpusParser = 
      Wos {_wos_limit :: Int}
    | Csv {_csv_limit :: Int}
    deriving (Show,Generic,Eq) 


data Proximity = 
      WeightedLogJaccard 
      { _wlj_sensibility   :: Double 
      , _wlj_thresholdInit :: Double
      , _wlj_thresholdStep :: Double }
    | Hamming 
    deriving (Show,Generic,Eq) 


data Synchrony = 
      ByProximityThreshold 
      { _bpt_threshold :: Double 
      , _bpt_sensibility :: Double}
    | ByProximityDistribution
      { _bpd_sensibility :: Double} 
    deriving (Show,Generic,Eq)     


data TimeUnit = 
      Year 
      { _year_period :: Int
      , _year_step   :: Int
      , _year_matchingFrame :: Int }
      deriving (Show,Generic,Eq) 


data ContextualUnit = 
      Fis 
      { _fis_support :: Int
      , _fis_size    :: Int }
    | MaxClique
      { _clique_size :: Int } 
      deriving (Show,Generic,Eq)      


data Quality = 
     Quality { _qua_granularity :: Double
             , _qua_minBranch   :: Int }
      deriving (Show,Generic,Eq)   


data Config = 
     Config { corpusPath     :: FilePath
            , listPath       :: FilePath
            , outputPath     :: FilePath
            , corpusParser   :: CorpusParser
            , phyloName      :: Text
            , phyloLevel     :: Int
            , phyloProximity :: Proximity
            , phyloSynchrony :: Synchrony
            , phyloQuality   :: Quality
            , timeUnit       :: TimeUnit
            , contextualUnit :: ContextualUnit
            , exportLabel    :: [PhyloLabel]
            , exportSort     :: Sort
            , exportFilter   :: [Filter]  
            } deriving (Show,Generic,Eq)


defaultConfig :: Config
defaultConfig = 
     Config { corpusPath     = ""
            , listPath       = ""
            , outputPath     = ""
            , corpusParser   = Csv 1000
            , phyloName      = pack "Default Phylo"
            , phyloLevel     = 2
            , phyloProximity = WeightedLogJaccard 10 0 0.1
            , phyloSynchrony = ByProximityDistribution 0
            , phyloQuality   = Quality 1 1
            , timeUnit       = Year 3 1 5
            , contextualUnit = Fis 1 5
            , exportLabel    = [BranchLabel MostInclusive 2, GroupLabel MostEmergentInclusive 2]
            , exportSort     = ByHierarchy
            , exportFilter   = [ByBranchSize 2]  
            }

instance FromJSON Config
instance ToJSON Config
instance FromJSON CorpusParser
instance ToJSON CorpusParser
instance FromJSON Proximity
instance ToJSON Proximity
instance FromJSON TimeUnit
instance ToJSON TimeUnit
instance FromJSON ContextualUnit
instance ToJSON ContextualUnit
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
instance FromJSON Synchrony
instance ToJSON Synchrony
instance FromJSON Quality
instance ToJSON Quality


-- | Software parameters
data Software =
     Software { _software_name    :: Text
              , _software_version :: Text
     } deriving (Generic, Show, Eq)

defaultSoftware :: Software
defaultSoftware = 
      Software { _software_name    = pack "Gargantext"
               , _software_version = pack "v4" }


-- | Global parameters of a Phylo
data PhyloParam =
     PhyloParam { _phyloParam_version  :: Text
                , _phyloParam_software :: Software
                , _phyloParam_config   :: Config
     } deriving (Generic, Show, Eq)

defaultPhyloParam :: PhyloParam
defaultPhyloParam =
      PhyloParam { _phyloParam_version  = pack "v2.adaptative"
                 , _phyloParam_software = defaultSoftware
                 , _phyloParam_config   = defaultConfig }


------------------
-- | Document | --
------------------


-- | Date : a simple Integer
type Date = Int

-- | Ngrams : a contiguous sequence of n terms
type Ngrams = Text

-- | Document : a piece of Text linked to a Date
data Document = Document
      { date :: Date
      , text :: [Ngrams]
      } deriving (Eq,Show,Generic,NFData)  


--------------------
-- | Foundation | --
--------------------


-- | The Foundations of a Phylo created from a given TermList 
data PhyloFoundations = PhyloFoundations
      { _foundations_roots   :: !(Vector Ngrams)
      , _foundations_mapList :: TermList
      } deriving (Generic, Show, Eq)


---------------------------
-- | Coocurency Matrix | --
---------------------------


-- | Cooc : a coocurency matrix between two ngrams
type Cooc =  Map (Int,Int) Double


-------------------
-- | Phylomemy | --
-------------------


-- | Phylo datatype of a phylomemy
--  foundations : the foundations of the phylo
--  timeCooc : a Map of coocurency by minimal unit of time (ex: by year)
--  timeDocs : a Map with the numbers of docs by minimal unit of time (ex: by year)
--  param : the parameters of the phylomemy (with the user's configuration)
--  periods : the temporal steps of a phylomemy
data Phylo =
     Phylo { _phylo_foundations :: PhyloFoundations
           , _phylo_timeCooc    :: !(Map Date Cooc)
           , _phylo_timeDocs    :: !(Map Date Double)
           , _phylo_termFreq    :: !(Map Int Double)
           , _phylo_param       :: PhyloParam
           , _phylo_periods     :: Map PhyloPeriodId PhyloPeriod
           }
           deriving (Generic, Show, Eq)


-- | PhyloPeriodId : the id of a given period
type PhyloPeriodId = (Date,Date)

-- | PhyloPeriod : steps of a phylomemy on a temporal axis
--  id: tuple (start date, end date) of the temporal step of the phylomemy
--  levels: levels of granularity
data PhyloPeriod =
     PhyloPeriod { _phylo_periodPeriod :: (Date,Date)
                 , _phylo_periodLevels :: Map PhyloLevelId PhyloLevel
                 } deriving (Generic, Show, Eq)   


-- | Level : a level of clustering
type Level = Int

-- | PhyloLevelId : the id of a level of clustering in a given period 
type PhyloLevelId  = (PhyloPeriodId,Level)

-- | PhyloLevel : levels of phylomemy on a synchronic axis
-- Levels description:
-- Level 0: The foundations and the base of the phylo
-- Level 1: First level of clustering (the Fis)
-- Level [2..N]: Nth level of synchronic clustering (cluster of Fis)
data PhyloLevel =
     PhyloLevel { _phylo_levelPeriod :: (Date,Date)
                , _phylo_levelLevel  :: Level 
                , _phylo_levelGroups :: Map PhyloGroupId PhyloGroup
                } 
                deriving (Generic, Show, Eq)   


type PhyloGroupId  = (PhyloLevelId, Int)

-- | BranchId : (a level, a sequence of branch index)
-- the sequence is a path of heritage from the most to the less specific branch
type PhyloBranchId = (Level, [Int])

-- | PhyloGroup : group of ngrams at each level and period
data PhyloGroup = 
      PhyloGroup { _phylo_groupPeriod   :: (Date,Date)
                 , _phylo_groupLevel    :: Level
                 , _phylo_groupIndex    :: Int
                 , _phylo_groupLabel    :: Text
                 , _phylo_groupSupport  :: Support
                 , _phylo_groupNgrams   :: [Int]
                 , _phylo_groupCooc     :: !(Cooc)
                 , _phylo_groupBranchId :: PhyloBranchId
                 , _phylo_groupMeta     :: Map Text [Double]
                 , _phylo_groupLevelParents  :: [Pointer]
                 , _phylo_groupLevelChilds   :: [Pointer]
                 , _phylo_groupPeriodParents :: [Pointer]
                 , _phylo_groupPeriodChilds  :: [Pointer]
                 }
                 deriving (Generic, Show, Eq, NFData)

-- | Weight : A generic mesure that can be associated with an Id
type Weight = Double

-- | Pointer : A weighted pointer to a given PhyloGroup
type Pointer = (PhyloGroupId, Weight)

data Filiation = ToParents | ToChilds deriving (Generic, Show)    
data PointerType = TemporalPointer | LevelPointer deriving (Generic, Show)                


---------------------------
-- | Frequent Item Set | --
---------------------------

-- | Support : Number of Documents where a Clique occurs
type Support  = Int

data PhyloCUnit = PhyloCUnit
  { _phyloCUnit_nodes   :: Set Ngrams
  , _phyloCUnit_support :: Support
  , _phyloCUnit_period  :: (Date,Date)
  } deriving (Generic,NFData,Show,Eq)


----------------
-- | Export | --
----------------

type DotId = TextLazy.Text

data EdgeType = GroupToGroup | BranchToGroup | BranchToBranch | PeriodToPeriod deriving (Show,Generic,Eq)

data Filter = ByBranchSize { _branch_size :: Double } deriving (Show,Generic,Eq)

data Order = Asc | Desc deriving (Show,Generic,Eq)

data Sort = ByBirthDate { _sort_order :: Order } | ByHierarchy deriving (Show,Generic,Eq)

data Tagger = MostInclusive | MostEmergentInclusive deriving (Show,Generic,Eq)

data PhyloLabel = 
      BranchLabel
      { _branch_labelTagger :: Tagger
      , _branch_labelSize   :: Int }
    | GroupLabel
      { _group_labelTagger  :: Tagger
      , _group_labelSize    :: Int }
    deriving (Show,Generic,Eq)

data PhyloBranch =
      PhyloBranch
      { _branch_id :: PhyloBranchId
      , _branch_label   :: Text
      , _branch_meta    :: Map Text [Double]
      } deriving (Generic, Show)

data PhyloExport =
      PhyloExport
      { _export_groups   :: [PhyloGroup]
      , _export_branches :: [PhyloBranch]
      } deriving (Generic, Show)

----------------
-- | Lenses | --
----------------

makeLenses ''Config
makeLenses ''Proximity
makeLenses ''Quality
makeLenses ''ContextualUnit
makeLenses ''PhyloLabel
makeLenses ''TimeUnit
makeLenses ''PhyloFoundations
makeLenses ''PhyloCUnit
makeLenses ''Phylo
makeLenses ''PhyloPeriod
makeLenses ''PhyloLevel
makeLenses ''PhyloGroup
makeLenses ''PhyloParam
makeLenses ''PhyloExport
makeLenses ''PhyloBranch

------------------------
-- | JSON instances | --
------------------------


$(deriveJSON (unPrefix "_foundations_"  ) ''PhyloFoundations)