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
import Data.Text   (Text)
import Data.Vector (Vector)

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


data CorpusParser = Wos | Csv deriving (Show,Generic) 

data Config = 
     Config { corpusPath   :: FilePath
            , listPath     :: FilePath
            , outputPath   :: FilePath
            , corpusParser :: CorpusParser
            , corpusLimit  :: Int
            , phyloName    :: Text
            , phyloLevel   :: Int
            , timePeriod   :: Int
            , timeStep     :: Int
            , fisSupport   :: Int
            , fisSize      :: Int
            , branchSize   :: Int
            , safeParall   :: Bool   
            } deriving (Show,Generic)

instance FromJSON Config
instance ToJSON Config
instance FromJSON CorpusParser
instance ToJSON CorpusParser


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
      } deriving (Show,Generic,NFData)  


--------------------
-- | Foundation | --
--------------------


-- | The Foundations of a Phylo created from a given TermList 
data PhyloFoundations = PhyloFoundations
      { _foundations_roots   :: !(Vector Ngrams)
      , _foundations_mapList :: TermList
      } deriving (Generic, Show, Eq)


----------------
-- | Lenses | --
----------------

makeLenses ''PhyloFoundations

------------------------
-- | JSON instances | --
------------------------


$(deriveJSON (unPrefix "_foundations_"  ) ''PhyloFoundations)