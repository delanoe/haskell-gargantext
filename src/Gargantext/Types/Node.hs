{-|
Module      : Gargantext.Types.Nodes
Description : Main Types of Nodes
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

-- {-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Types.Node where

import Gargantext.Prelude

import Text.Show (Show())
import Data.Text (Text, unpack)
import Text.Read (read)
import GHC.Generics (Generic)
import Data.Eq (Eq)
import Data.Time (UTCTime)
import Gargantext.Utils.Prefix (unPrefix)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson
import Servant
import Data.Either

import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)

-- Instances:
import Data.Time.Segment (jour)
import Data.Aeson (Value(),toJSON)


------------------------------------------------------------------------
data Status  = Status { status_Date     :: Maybe UTCTime
                      , status_Error    :: Maybe Text
                      , status_Action   :: Maybe Text
                      , status_Complete :: Maybe Bool
                      , status_Progress :: Maybe Int
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "status_") ''Status)

instance Arbitrary Status where
    arbitrary = elements [Status Nothing Nothing Nothing Nothing Nothing]


------------------------------------------------------------------------
data HyperdataDocument = HyperdataDocument { hyperdataDocument_Bdd                :: Maybe Text
                                           , hyperdataDocument_Doi                :: Maybe Text
                                           , hyperdataDocument_Url                :: Maybe Text
                                           , hyperdataDocument_Page               :: Maybe Int
                                           , hyperdataDocument_Title              :: Maybe Text
                                           , hyperdataDocument_Authors            :: Maybe Text
                                           , hyperdataDocument_Abstract           :: Maybe Text
                                           , hyperdataDocument_Statuses           :: Maybe [Status]
                                           , hyperdataDocument_Publication_date   :: Maybe Text
                                           , hyperdataDocument_Publication_year   :: Maybe Double
                                           , hyperdataDocument_Publication_month  :: Maybe Double
                                           , hyperdataDocument_Publication_hour   :: Maybe Double
                                           , hyperdataDocument_Publication_minute :: Maybe Double
                                           , hyperdataDocument_Publication_second :: Maybe Double
                                           , hyperdataDocument_LanguageIso2       :: Maybe Text
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDocument_") ''HyperdataDocument)

hyperdataDocuments :: [HyperdataDocument]
hyperdataDocuments = [HyperdataDocument Nothing Nothing Nothing Nothing (Just "Title") 
                                            Nothing (Just "Abstract") Nothing Nothing 
                                            Nothing Nothing Nothing Nothing Nothing Nothing
                         ]


instance Arbitrary HyperdataDocument where
    arbitrary = elements hyperdataDocuments

------------------------------------------------------------------------
data LanguageNodes = LanguageNodes { languageNodes___unknown__ :: [Int]}
    deriving (Show, Generic)
$(deriveJSON (unPrefix "languageNodes_") ''LanguageNodes)


------------------------------------------------------------------------

data Resource = Resource { resource_Url  :: Maybe Text
                         , resource_Path :: Maybe Text
                         , resource_Type :: Maybe Int
                         , resource_Extracted :: Maybe Bool
                         } deriving (Show, Generic)
$(deriveJSON (unPrefix "resource_") ''Resource)

instance Arbitrary Resource where
    arbitrary = elements [Resource Nothing Nothing Nothing Nothing]

data HyperdataCorpus = HyperdataCorpus { hyperdataCorpus_Action       :: Maybe Text
                                       , hyperdataCorpus_Statuses     :: Maybe [Status]
                                       , hyperdataCorpus_Languages    :: Maybe LanguageNodes
                                       , hyperdataCorpus_Resources    :: Maybe [Resource]
                                       , hyperdataCorpus_Language_id  :: Maybe Text
                                       , hyperdataCorpus_Skipped_docs :: Maybe [Int]
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataCorpus_") ''HyperdataCorpus)


data HyperdataUser = HyperdataUser { hyperdataUser_language       :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataUser_") ''HyperdataUser)

-- Preferences ?

data HyperdataFolder = HyperdataFolder { hyperdataFolder_Preferences   :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFolder_") ''HyperdataFolder)


data HyperdataProject = HyperdataProject { hyperdataProject_Preferences   :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataProject_") ''HyperdataProject)



data HyperdataList = HyperdataList { hyperdataList_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataList_") ''HyperdataList)

data HyperdataScore = HyperdataScore { hyperdataScore_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataScore_") ''HyperdataScore)



data HyperdataFavorites = HyperdataFavorites { hyperdataFavorites_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFavorites_") ''HyperdataFavorites)

data HyperdataResource = HyperdataResource { hyperdataResource_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataResource_") ''HyperdataResource)



-- TODO add the Graph Structure here
data HyperdataGraph = HyperdataGraph { hyperdataGraph_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataGraph_") ''HyperdataGraph)


-- TODO add the Graph Structure here
data HyperdataPhylo = HyperdataPhylo { hyperdataPhylo_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataPhylo_") ''HyperdataPhylo)

-- | TODO FEATURE: Notebook saved in the node (to work with Python or Haskell)
data HyperdataNotebook = HyperdataNotebook { hyperdataNotebook_Preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataNotebook_") ''HyperdataNotebook)



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

data NodeType = NodeUser | Project | Corpus | Document | DocumentCopy
              | Classification
              | Lists
              | Metrics | Occurrences
              deriving (Show, Read, Eq, Generic)

instance FromJSON NodeType
instance ToJSON NodeType
instance FromHttpApiData NodeType where parseUrlPiece = Right . read . unpack


------------------------------------------------------------------------
data NodePoly id typename userId parentId name date hyperdata = Node { node_id        :: id
                                                                     , node_typename  :: typename
                                                                     , node_userId    :: userId
                                                                --   , nodeHashId    :: hashId
                                                                     , node_parentId  :: parentId
                                                                     , node_name      :: name
                                                                     , node_date      :: date
                                                                     , node_hyperdata :: hyperdata
                                                              --       , node_titleAbstract :: titleAbstract
                                                                     } deriving (Show)
$(deriveJSON (unPrefix "node_") ''NodePoly)

instance Arbitrary (NodePoly NodeId NodeTypeId (Maybe NodeUserId) NodeParentId NodeName UTCTime Value) where
    arbitrary = elements [Node 1 1 (Just 1) 1 "name" (jour 2018 01 01) (toJSON ("{}"::Text))]


instance Arbitrary (NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime Value) where
    arbitrary = elements [Node 1 1 1 (Just 1) "name" (jour 2018 01 01) (toJSON ("{}"::Text))]



