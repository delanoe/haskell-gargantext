{-|
Module      : Gargantext.Types.Nodes
Description : Main Types of Nodes
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Types.Node where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Aeson (Value(),toJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Either
import Data.Eq (Eq)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time.Segment (jour)
import Data.Swagger
import Data.Maybe (fromJust)

import Text.Read (read)
import Text.Show (Show())

import Servant

import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)

import Gargantext.Prelude
import Gargantext.Utils.Prefix (unPrefix)

------------------------------------------------------------------------
data Status  = Status { status_date     :: Maybe UTCTime
                      , status_error    :: Maybe Text
                      , status_action   :: Maybe Text
                      , status_complete :: Maybe Bool
                      , status_progress :: Maybe Int
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "status_") ''Status)

instance Arbitrary Status where
    arbitrary = elements [Status Nothing Nothing Nothing Nothing Nothing]


------------------------------------------------------------------------
data HyperdataDocument = HyperdataDocument { hyperdataDocument_bdd                :: Maybe Text
                                           , hyperdataDocument_doi                :: Maybe Text
                                           , hyperdataDocument_url                :: Maybe Text
                                           , hyperdataDocument_page               :: Maybe Int
                                           , hyperdataDocument_title              :: Maybe Text
                                           , hyperdataDocument_authors            :: Maybe Text
                                           , hyperdataDocument_abstract           :: Maybe Text
                                           , hyperdataDocument_statuses           :: Maybe [Status]
                                           , hyperdataDocument_publication_date   :: Maybe Text
                                           , hyperdataDocument_publication_year   :: Maybe Double
                                           , hyperdataDocument_publication_month  :: Maybe Double
                                           , hyperdataDocument_publication_hour   :: Maybe Double
                                           , hyperdataDocument_publication_minute :: Maybe Double
                                           , hyperdataDocument_publication_second :: Maybe Double
                                           , hyperdataDocument_languageIso2       :: Maybe Text
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

------------------------------------------------------------------------
data NodeType = NodeUser | Project | Corpus | Document | DocumentCopy
              | Classification
              | Lists
              | Metrics | Occurrences
              deriving (Show, Read, Eq, Generic)

instance FromJSON NodeType
instance ToJSON NodeType

instance FromHttpApiData NodeType 
  where 
      parseUrlPiece = Right . read . unpack

instance ToParamSchema NodeType
instance ToSchema      NodeType

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
                                                                     } deriving (Show, Generic)
$(deriveJSON (unPrefix "node_") ''NodePoly)

instance Arbitrary (NodePoly NodeId NodeTypeId (Maybe NodeUserId) NodeParentId NodeName UTCTime Value) where
    arbitrary = elements [Node 1 1 (Just 1) 1 "name" (jour 2018 01 01) (toJSON ("{}"::Text))]


instance Arbitrary (NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime Value) where
    arbitrary = elements [Node 1 1 1 (Just 1) "name" (jour 2018 01 01) (toJSON ("{}"::Text))]

instance Arbitrary (NodePoly NodeId NodeTypeId (Maybe NodeUserId) NodeParentId NodeName UTCTime HyperdataDocument) where
    arbitrary = elements [Node 1 1 (Just 1) 1 "name" (jour 2018 01 01) ((hyperdataDocument))]


instance Arbitrary (NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime HyperdataDocument) where
    arbitrary = elements [Node 1 1 1 (Just 1) "name" (jour 2018 01 01) hyperdataDocument]

hyperdataDocument :: HyperdataDocument
hyperdataDocument = fromJust $ decode "{\"publication_day\":6,\"language_iso2\":\"en\",\"publication_minute\":0,\"publication_month\":7,\"language_iso3\":\"eng\",\"publication_second\":0,\"authors\":\"Nils Hovdenak, Kjell Haram\",\"publication_year\":2012,\"publication_date\":\"2012-07-06 00:00:00+00:00\",\"language_name\":\"English\",\"statuses\":[],\"realdate_full_\":\"2012 01 12\",\"source\":\"European journal of obstetrics, gynecology, and reproductive biology\",\"abstract\":\"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome.\",\"title\":\"Influence of mineral and vitamin supplements on pregnancy outcome.\",\"publication_hour\":0}"


instance ToSchema HyperdataDocument where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema (NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime HyperdataDocument)
instance ToSchema (NodePoly NodeId NodeTypeId (Maybe NodeUserId) NodeParentId NodeName UTCTime HyperdataDocument)

instance ToSchema Status


