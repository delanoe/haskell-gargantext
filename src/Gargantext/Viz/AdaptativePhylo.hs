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


----------------
-- | Config | --
----------------  


data CorpusParser = Wos | Csv deriving (Show,Generic,Eq) 

data Config = 
     Config { corpusPath   :: FilePath
            , listPath     :: FilePath
            , outputPath   :: FilePath
            , corpusParser :: CorpusParser
            , corpusLimit  :: Int
            , phyloName    :: Text
            , phyloLevel   :: Int
            , timeUnit     :: Int
            , timePeriod   :: Int
            , timeStep     :: Int
            , fisSupport   :: Int
            , fisSize      :: Int
            , branchSize   :: Int  
            } deriving (Show,Generic,Eq)

defaultConfig :: Config
defaultConfig = 
     Config { corpusPath   = ""
            , listPath     = ""
            , outputPath   = ""
            , corpusParser = Csv
            , corpusLimit  = 1000
            , phyloName    = pack "Default Phylo"
            , phyloLevel   = 2
            , timeUnit     = 1
            , timePeriod   = 3
            , timeStep     = 1
            , fisSupport   = 2
            , fisSize      = 4
            , branchSize   = 3  
            }

instance FromJSON Config
instance ToJSON Config
instance FromJSON CorpusParser
instance ToJSON CorpusParser


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
           , _phylo_param       :: PhyloParam
           , _phylo_periods     :: [PhyloPeriod]
           }
           deriving (Generic, Show, Eq)


-- | PhyloPeriodId : the id of a given period
type PhyloPeriodId = (Date,Date)

-- | PhyloPeriod : steps of a phylomemy on a temporal axis
--  id: tuple (start date, end date) of the temporal step of the phylomemy
--  levels: levels of granularity
data PhyloPeriod =
     PhyloPeriod { _phylo_periodId     :: PhyloPeriodId
                 , _phylo_periodLevels :: [PhyloLevel]
                 }
                 deriving (Generic, Show, Eq)   


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
     PhyloLevel { _phylo_levelId     :: PhyloLevelId
                , _phylo_levelGroups :: [PhyloGroup]
                }
                deriving (Generic, Show, Eq)   


--------------------
-- | PhyloGroup | --
-------------------- 


type Index = Int
type PhyloGroupId  = (PhyloLevelId, Index)

-- | PhyloGroup : group of ngrams at each level and period
data PhyloGroup = 
      PhyloGroup { _phylo_groupId :: PhyloGroupId
                 }
                 deriving (Generic, Show, Eq)


---------------------------
-- | Frequent Item Set | --
---------------------------

-- | Clique : Set of ngrams cooccurring in the same Document
type Clique   = Set Ngrams

-- | Support : Number of Documents where a Clique occurs
type Support  = Int

-- | Fis : Frequent Items Set (ie: the association between a Clique and a Support)
data PhyloFis = PhyloFis
  { _phyloFis_clique  :: Clique
  , _phyloFis_support :: Support
  , _phyloFis_period  :: (Date,Date)
  } deriving (Generic,NFData,Show,Eq)


----------------
-- | Lenses | --
----------------

makeLenses ''Config
makeLenses ''PhyloFoundations
makeLenses ''PhyloFis
makeLenses ''Phylo
makeLenses ''PhyloPeriod
makeLenses ''PhyloLevel
makeLenses ''PhyloGroup
makeLenses ''PhyloParam

------------------------
-- | JSON instances | --
------------------------


$(deriveJSON (unPrefix "_foundations_"  ) ''PhyloFoundations)