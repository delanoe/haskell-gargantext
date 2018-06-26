{-|
Module      : Gargantext.Core.Types.Nodes
Description : Main Types of Nodes
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Core.Types.Node where

import Prelude (Enum, Bounded, minBound, maxBound)

import GHC.Generics (Generic)

import           Control.Lens hiding (elements)
import qualified Control.Lens   as L
import           Control.Applicative ((<*>))

import           Data.Aeson
import           Data.Aeson (Value(),toJSON)
import           Data.Aeson.TH (deriveJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.Either
import           Data.Eq (Eq)
import           Data.Text (Text, unpack)
import           Data.Time (UTCTime)
import           Data.Time.Segment (jour, timesAfter, Granularity(D))
import           Data.Swagger

import           Text.Read (read)
import           Text.Show (Show())

import           Servant

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck (elements)

import           Gargantext.Prelude
import           Gargantext.Core.Utils.Prefix (unPrefix)

------------------------------------------------------------------------

type UTCTime' = UTCTime

instance Arbitrary UTCTime' where
    arbitrary = elements $ timesAfter 100 D (jour 2000 01 01)



------------------------------------------------------------------------
data Status  = Status { status_failed    :: Int
                      , status_succeeded :: Int
                      , status_remaining :: Int
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "status_") ''Status)

instance Arbitrary Status where
  arbitrary = Status <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------
data HyperdataDocument = HyperdataDocument { hyperdataDocument_bdd                :: Maybe Text
                                           , hyperdataDocument_doi                :: Maybe Int
                                           , hyperdataDocument_url                :: Maybe Text
                                           , hyperdataDocument_page               :: Maybe Int
                                           , hyperdataDocument_title              :: Maybe Text
                                           , hyperdataDocument_authors            :: Maybe Text
                                           , hyperdataDocument_source             :: Maybe Text
                                           , hyperdataDocument_abstract           :: Maybe Text
                                           , hyperdataDocument_statuses           :: Maybe [Status]
                                           , hyperdataDocument_publication_date   :: Maybe Text
                                           , hyperdataDocument_publication_year   :: Maybe Int
                                           , hyperdataDocument_publication_month  :: Maybe Int
                                           , hyperdataDocument_publication_hour   :: Maybe Int
                                           , hyperdataDocument_publication_minute :: Maybe Int
                                           , hyperdataDocument_publication_second :: Maybe Int
                                           , hyperdataDocument_languageIso2       :: Maybe Text
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDocument_") ''HyperdataDocument)

toHyperdataDocuments :: [(Text, Text)] -> [HyperdataDocument]
toHyperdataDocuments ts = map (\(t1,t2) -> HyperdataDocument Nothing Nothing Nothing Nothing (Just t1) 
                                           Nothing (Just t2) Nothing Nothing Nothing 
                                           Nothing Nothing Nothing Nothing Nothing Nothing
                                           ) ts

hyperdataDocuments :: [HyperdataDocument]
hyperdataDocuments = toHyperdataDocuments [ ("AI is big but less than crypto", "Troll System journal")
                                          , ("Crypto is big but less than AI", "System Troll review" )
                                          , ("Science is magic"              , "Closed Source review")
                                          , ("Open science for all"          , "No Time"             )
                                          , ("Closed science for me"         , "No Space"            )
                                          ]


instance Arbitrary HyperdataDocument where
    arbitrary = elements hyperdataDocuments

------------------------------------------------------------------------
data LanguageNodes = LanguageNodes { languageNodes___unknown__ :: [Int]}
    deriving (Show, Generic)
$(deriveJSON (unPrefix "languageNodes_") ''LanguageNodes)

------------------------------------------------------------------------
-- level: debug | dev  (fatal = critical)
data EventLevel = CRITICAL | FATAL | ERROR | WARNING | INFO | DEBUG
  deriving (Show, Generic, Enum, Bounded)

instance FromJSON EventLevel
instance ToJSON EventLevel

instance Arbitrary EventLevel where
  arbitrary = elements [minBound..maxBound]

------------------------------------------------------------------------

data Event = Event { event_level   :: EventLevel
                   , event_message :: Text
                   , event_date    :: UTCTime
            } deriving (Show, Generic)
$(deriveJSON (unPrefix "event_") ''Event)

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------

type Text' = Text

instance Arbitrary Text' where
  arbitrary = elements ["ici", "la"]

data Resource = Resource { resource_path    :: Maybe Text
                         , resource_scraper :: Maybe Text
                         , resource_query   :: Maybe Text
                         , resource_events  :: [Event]
                         , resource_status  :: Status
                         , resource_date    :: UTCTime'
                         } deriving (Show, Generic)
$(deriveJSON (unPrefix "resource_") ''Resource)

instance Arbitrary Resource where
    arbitrary = Resource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------

data HyperdataCorpus = HyperdataCorpus { hyperdataCorpus_resources    :: [Resource]
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataCorpus_") ''HyperdataCorpus)


data HyperdataUser = HyperdataUser { hyperdataUser_language       :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataUser_") ''HyperdataUser)

-- Preferences ?

data HyperdataFolder = HyperdataFolder { hyperdataFolder_preferences   :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFolder_") ''HyperdataFolder)


data HyperdataProject = HyperdataProject { hyperdataProject_preferences   :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataProject_") ''HyperdataProject)



data HyperdataList = HyperdataList { hyperdataList_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataList_") ''HyperdataList)

data HyperdataScore = HyperdataScore { hyperdataScore_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataScore_") ''HyperdataScore)



data HyperdataFavorites = HyperdataFavorites { hyperdataFavorites_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFavorites_") ''HyperdataFavorites)

data HyperdataResource = HyperdataResource { hyperdataResource_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataResource_") ''HyperdataResource)



-- TODO add the Graph Structure here
data HyperdataGraph = HyperdataGraph { hyperdataGraph_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataGraph_") ''HyperdataGraph)


-- TODO add the Graph Structure here
data HyperdataPhylo = HyperdataPhylo { hyperdataPhylo_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataPhylo_") ''HyperdataPhylo)

-- | TODO FEATURE: Notebook saved in the node (to work with Python or Haskell)
data HyperdataNotebook = HyperdataNotebook { hyperdataNotebook_preferences   :: Maybe Text
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
data NodeType = NodeUser | Project | Folder | Corpus | Annuaire | Document | UserPage | DocumentCopy | Favorites
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


------------------------------------------------------------------------
hyperdataDocument :: HyperdataDocument
hyperdataDocument = case decode docExample of
                      Just hp -> hp
                      Nothing -> HyperdataDocument Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
docExample :: ByteString
docExample = "{\"publication_day\":6,\"language_iso2\":\"en\",\"publication_minute\":0,\"publication_month\":7,\"language_iso3\":\"eng\",\"publication_second\":0,\"authors\":\"Nils Hovdenak, Kjell Haram\",\"publication_year\":2012,\"publication_date\":\"2012-07-06 00:00:00+00:00\",\"language_name\":\"English\",\"statuses\":[],\"realdate_full_\":\"2012 01 12\",\"source\":\"European journal of obstetrics, gynecology, and reproductive biology\",\"abstract\":\"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome.\",\"title\":\"Influence of mineral and vitamin supplements on pregnancy outcome.\",\"publication_hour\":0}"


instance ToSchema HyperdataDocument where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    L.& mapped.schema.description ?~ "a document"
    L.& mapped.schema.example ?~ toJSON hyperdataDocument


instance ToSchema Value where
  declareNamedSchema proxy = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy
    L.& mapped.schema.description ?~ "a document"
    L.& mapped.schema.example ?~ toJSON ("" :: Text)


instance ToSchema (NodePoly NodeId NodeTypeId NodeUserId 
                            (Maybe NodeParentId) NodeName
                            UTCTime HyperdataDocument
                  )

instance ToSchema (NodePoly NodeId NodeTypeId 
                            (Maybe NodeUserId) 
                            NodeParentId NodeName 
                            UTCTime HyperdataDocument
                  )

instance ToSchema (NodePoly NodeId NodeTypeId 
                            (Maybe NodeUserId) 
                            NodeParentId NodeName 
                            UTCTime Value
                  )

instance ToSchema (NodePoly NodeId NodeTypeId 
                            (NodeUserId) 
                            (Maybe NodeParentId) NodeName 
                            UTCTime Value
                  )


instance ToSchema Status


