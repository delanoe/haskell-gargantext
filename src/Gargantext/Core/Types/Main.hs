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
{-# LANGUAGE TemplateHaskell   #-}

-----------------------------------------------------------------------
module Gargantext.Core.Types.Main where
------------------------------------------------------------------------

import Prelude (Enum, Bounded, minBound, maxBound)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson as A
import Data.Aeson.TH (deriveJSON)
import Data.Map (fromList, lookup)
import Data.Either (Either(..))
import Data.Eq (Eq())
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Swagger

import Gargantext.Database.Types.Node  -- (NodeType(..), Node, Hyperdata(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

import GHC.Generics (Generic)
import Servant.API (FromHttpApiData(..))
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Text.Read (read)

------------------------------------------------------------------------
data NodeTree = NodeTree { _nt_name :: Text
                         , _nt_type :: NodeType
                         , _nt_id   :: NodeId
                         } deriving (Show, Read, Generic)

$(deriveJSON (unPrefix "_nt_") ''NodeTree)
------------------------------------------------------------------------

-- Garg Network is a network of all Garg nodes
--gargNetwork = undefined

-- | Garg Node is Database Schema Typed as specification
-- gargNode gathers all the Nodes of all users on one Node
gargNode :: [Tree NodeTree]
gargNode = [userTree]

-- | User Tree simplified
userTree :: Tree NodeTree
userTree = TreeN (NodeTree "user name" NodeUser 1) [annuaireTree, projectTree]

-- | Project Tree
projectTree :: Tree NodeTree
projectTree = TreeN (NodeTree "Project CNRS/IMT" NodeFolder 2) [corpusTree 10 "A", corpusTree 20 "B"]

-- | Corpus Tree
annuaireTree :: Tree NodeTree
annuaireTree = (leafT $ NodeTree "Annuaire" NodeAnnuaire 41)

corpusTree :: NodeId -> Text -> Tree NodeTree
corpusTree nId t  = TreeN (NodeTree ("Corpus " <> t)  NodeCorpus nId) (  [ leafT $ NodeTree "Dashboard" NodeDashboard (nId +1)
                                                                         , leafT $ NodeTree "Graph" NodeGraph (nId +2)
                                                                         ]
--                                                      <> [ leafT $ NodeTree "My lists"  Lists    5]
--                          <> [ leafT (NodeTree "Metrics A" Metrics 6)  ]
--                          <> [ leafT (NodeTree "Class A" Classification 7)]
                          )


--data Classification = Favorites | MyClassifcation

type HashId   = Text

type TypeId     = Int
-- TODO multiple ListType declaration, remove it
data ListType  =  StopTerm | CandidateTerm | GraphTerm
  deriving (Generic, Eq, Ord, Show, Read, Enum, Bounded)

instance ToJSON   ListType
instance FromJSON ListType
instance ToSchema ListType
instance ToParamSchema ListType
instance Arbitrary ListType where
  arbitrary = elements [minBound..maxBound]

instance FromHttpApiData ListType where
  parseUrlPiece = Right . read . unpack

type ListTypeId = Int

listTypeId :: ListType -> ListTypeId
listTypeId StopTerm      = 0
listTypeId CandidateTerm = 1
listTypeId GraphTerm       = 2

fromListTypeId :: ListTypeId -> Maybe ListType
fromListTypeId i = lookup i $ fromList [ (listTypeId l, l) | l <- [minBound..maxBound]]

-- data Metrics = Occurrences | Cooccurrences | Specclusion | Genclusion | Cvalue
--              | TfidfCorpus | TfidfGlobal   | TirankLocal | TirankGlobal

-- | Community Manager Use Case
type Annuaire  = NodeCorpus

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
-- Temporary types to be removed
type ErrorMessage = Text

-- Queries
type Limit    = Int
type Offset   = Int


------------------------------------------------------------------------
-- All the Database is structred like a hierarchical Tree
data Tree a = TreeN a [Tree a]
  deriving (Show, Read, Eq, Generic, Ord)

instance ToJSON a => ToJSON   (Tree a) where
  toJSON (TreeN node nodes) =
    object ["node" A..= toJSON node, "children" A..= toJSON nodes]

instance FromJSON a => FromJSON (Tree a)

instance ToSchema NodeTree
instance ToSchema a => ToSchema  (Tree a)
instance Arbitrary (Tree NodeTree) where
  arbitrary = elements [userTree, userTree]


-- data Tree a = NodeT a [Tree a]
-- same as Data.Tree
leafT :: a -> Tree a
leafT x = TreeN x []
