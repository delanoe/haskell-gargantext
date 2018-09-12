{-|
Module      : Gargantext.Core.Types.Main
Description : Short description
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------
module Gargantext.Core.Types.Main where
------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq())
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Swagger

import Gargantext.Database.Types.Node
import Gargantext.Prelude

import GHC.Generics (Generic)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
-- All the Database is structred like a hierarchical Tree
data Tree a = NodeT a [Tree a]
  deriving (Show, Read, Eq, Generic)

instance ToJSON   (Tree NodeType)
instance FromJSON (Tree NodeType)

instance ToSchema  (Tree NodeType)
instance Arbitrary (Tree NodeType) where
  arbitrary = elements [userTree, userTree]

-- data Tree a = NodeT a [Tree a]
-- same as Data.Tree
leafT :: a -> Tree a
leafT x = NodeT x []

-- Garg Network is a network of all Garg nodes
--gargNetwork = undefined

-- | Garg Node is Database Schema Typed as specification
-- gargNode gathers all the Nodes of all users on one Node
gargNode :: [Tree NodeType]
gargNode = [userTree]

-- | User Tree simplified
userTree :: Tree NodeType
userTree = NodeT NodeUser [projectTree]

-- | Project Tree
projectTree :: Tree NodeType
projectTree = NodeT Project [corpusTree]

-- | Corpus Tree
corpusTree :: Tree NodeType
corpusTree = NodeT NodeCorpus (  [ leafT Document ]
                          <> [ leafT Lists    ]
                          <> [ leafT Metrics  ]
                          <> [ leafT Classification]
                          )

--   TODO make instances of Nodes
-- NP
-- - why NodeUser and not just User ?
-- - is this supposed to hold data ?
-- AD : Yes, some preferences for instances

data Parent = NodeType NodeId

--data Classification = Favorites | MyClassifcation

data Lists  =  StopList    | MainList | MapList | GroupList

-- data Metrics = Occurrences | Cooccurrences | Specclusion | Genclusion | Cvalue
--              | TfidfCorpus | TfidfGlobal   | TirankLocal | TirankGlobal


-- | Community Manager Use Case
type Annuaire  = NodeCorpus
type Individu  = Document

-- | Favorites Node enable Node categorization
type Favorites = Node HyperdataFavorites

-- | Favorites Node enable Swap Node with some synonyms for clarity
type NodeSwap  = Node HyperdataResource

-- | Then a Node can be a List which has some synonyms
type List = Node HyperdataList
type StopList   = List
type MainList   = List
type MapList    = List
type GroupList  = List

-- | Then a Node can be a Score which has some synonyms
type Score = Node HyperdataScore
type Occurrences   = Score
type Cooccurrences = Score
type Specclusion   = Score
type Genclusion    = Score
type Cvalue        = Score
type Tficf         = Score
---- TODO All these Tfidf* will be replaced with TFICF
type TfidfCorpus   = Tficf
type TfidfGlobal   = Tficf
type TirankLocal   = Tficf
type TirankGlobal  = Tficf
--
---- | Then a Node can be either a Graph or a Phylo or a Notebook
type Graph    = Node HyperdataGraph
type Phylo    = Node HyperdataPhylo
type Notebook = Node HyperdataNotebook


-- Temporary types to be removed
type ErrorMessage = Text

-- Queries
type ParentId = NodeId
type Limit    = Int
type Offset   = Int

