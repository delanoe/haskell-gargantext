{-|
Module      : .Gargantext.Types.Main
Description : Short description
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gargantext.Types.Main where

import Prelude

import Data.Eq (Eq())
import Data.Monoid ((<>))
import Protolude (fromMaybe)
import Data.Aeson
import GHC.Generics
import Servant
import Data.Text (unpack)
import Text.Read (read)
import Data.Either (Either(Right))
--import Data.ByteString (ByteString())
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.List (lookup)
import Gargantext.Types.Node ( NodePoly, HyperdataUser
                               , HyperdataFolder   , HyperdataCorpus   , HyperdataDocument
                               , HyperdataFavorites, HyperdataResource
                               , HyperdataList  , HyperdataScore
                               , HyperdataGraph
                               , HyperdataPhylo
                               , HyperdataNotebook
                               )


-- | Language of a Text
-- For simplicity, we suppose text has an homogenous language
data Language = EN | FR -- | DE | IT | SP
    -- > EN == english
    -- > FR == french
    -- > DE == deutch  (not implemented yet)
    -- > IT == italian (not implemented yet)
    -- > SP == spanish (not implemented yet)
    -- > ... add your language and help us to implement it (:


-- All the Database is structred like a hierarchical Tree
data Tree a = NodeT a [Tree a]
              deriving (Show, Read, Eq)


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
corpusTree = NodeT Corpus (  [ leafT Document ]
                          <> [ leafT Lists    ]
                          <> [ leafT Metrics  ]
                          <> [ leafT Classification]
                          )

--   TODO make instances of Nodes
-- NP
-- * why NodeUser and not just User ?
-- * is this supposed to hold data ?
data NodeType = NodeUser | Project | Corpus | Document | DocumentCopy
              | Classification
              | Lists
              | Metrics | Occurrences
              deriving (Show, Read, Eq, Generic)

instance FromJSON NodeType
instance ToJSON NodeType
instance FromHttpApiData NodeType where parseUrlPiece = Right . read . unpack

data Classification = Favorites | MyClassifcation

data Lists  =  StopList    | MainList | MapList | GroupList

-- data Metrics = Occurrences | Cooccurrences | Specclusion | Genclusion | Cvalue
--              | TfidfCorpus | TfidfGlobal   | TirankLocal | TirankGlobal



-- | NodePoly indicates that Node has a Polymorphism Type
type Node json   = NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime json -- NodeVector

-- type Node json   = NodePoly NodeId NodeTypeId UserId ParentId NodeName UTCTime json
type NodeTypeId   = Int
type NodeId       = Int
type NodeParentId = Int
type NodeUserId   = Int
type NodeName     = Text
--type NodeVector   = Vector

--type NodeUser    = Node HyperdataUser

-- | Then a Node can be either a Folder or a Corpus or a Document
type NodeUser = Node HyperdataUser
type Folder   = Node HyperdataFolder
type Project  = Folder -- NP Node HyperdataProject ?
type Corpus   = Node HyperdataCorpus
type Document = Node HyperdataDocument

-- | Community Manager Use Case
type Annuaire  = Corpus
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


nodeTypes :: [(NodeType, NodeTypeId)]
nodeTypes = [ (NodeUser      ,  1)
            , (Project       ,  2)
            , (Corpus        ,  3)
            , (Document      ,  4)
            --, (NodeSwap      , 19)
------  Lists
--            , (StopList      ,  5)
--            , (GroupList     ,  6)
--            , (MainList      ,  7)
--            , (MapList       ,  8)
----  Scores
            , (Occurrences   , 10)
--            , (Cooccurrences ,  9)
--
--            , (Specclusion   , 11)
--            , (Genclusion    , 18)
--            , (Cvalue       , 12)
--
--            , (TfidfCorpus  , 13)
--            , (TfidfGlobal  , 14)
--
--            , (TirankLocal  , 16)
--            , (TirankGlobal , 17)
--
----  Node management
--            , (Favorites     , 15)
--
            ]
--
nodeTypeId :: NodeType -> NodeTypeId
nodeTypeId tn = fromMaybe (error $ "Typename " <> show tn <> " does not exist")
                          (lookup tn nodeTypes)




-- Temporary types to be removed
type Ngrams = (Text, Text, Text)
type ErrorMessage = Text

-- Queries
type ParentId = NodeId
type Limit    = Int
type Offset    = Int


