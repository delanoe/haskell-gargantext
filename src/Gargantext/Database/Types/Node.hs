{-|
Module      : Gargantext.Database.Types.Nodes
Description : Main Types of Nodes in Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Database.Types.Node
  where

import Prelude (Enum, Bounded, minBound, maxBound)

import GHC.Generics (Generic)

import           Control.Lens hiding (elements)
import qualified Control.Lens   as L
import           Control.Applicative ((<*>))

import           Data.Aeson
import           Data.Aeson.Types (emptyObject)
import           Data.Aeson (Object, toJSON)
import           Data.Aeson.TH (deriveJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.Either
import           Data.Eq (Eq)
import           Data.Monoid (mempty)
import           Data.Text (Text, unpack)
import           Data.Time (UTCTime)
import           Data.Time.Segment (jour, timesAfter, Granularity(D))
import           Data.Swagger

import           Text.Read (read)
import           Text.Show (Show())

import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import           Servant

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck (elements)

import           Gargantext.Prelude
import           Gargantext.Core.Utils.Prefix (unPrefix)
------------------------------------------------------------------------
type NodeId = Int

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
data StatusV3  = StatusV3 { statusV3_error  :: Maybe Text
                          , statusV3_action :: Maybe Text
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "statusV3_") ''StatusV3)
------------------------------------------------------------------------

-- Only Hyperdata types should be member of this type class.
class Hyperdata a

------------------------------------------------------------------------
data HyperdataDocumentV3 = HyperdataDocumentV3 { hyperdataDocumentV3_publication_day    :: !(Maybe Int)
                                               , hyperdataDocumentV3_language_iso2      :: !(Maybe Text)
                                               , hyperdataDocumentV3_publication_second :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_minute :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_month  :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_hour   :: !(Maybe Int)
                                               , hyperdataDocumentV3_error              :: !(Maybe Text)
                                               , hyperdataDocumentV3_language_iso3      :: !(Maybe Text)
                                               , hyperdataDocumentV3_authors            :: !(Maybe Text)
                                               , hyperdataDocumentV3_publication_year   :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_date   :: !(Maybe Text)
                                               , hyperdataDocumentV3_language_name      :: !(Maybe Text)
                                               , hyperdataDocumentV3_statuses           :: !(Maybe [StatusV3])
                                               , hyperdataDocumentV3_realdate_full_     :: !(Maybe Text)
                                               , hyperdataDocumentV3_source             :: !(Maybe Text)
                                               , hyperdataDocumentV3_abstract           :: !(Maybe Text)
                                               , hyperdataDocumentV3_title              :: !(Maybe Text)
                                               } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDocumentV3_") ''HyperdataDocumentV3)

instance Hyperdata HyperdataDocumentV3
------------------------------------------------------------------------


data HyperdataDocument = HyperdataDocument { _hyperdataDocument_bdd                :: Maybe Text
                                           , _hyperdataDocument_doi                :: Maybe Text
                                           , _hyperdataDocument_url                :: Maybe Text
                                           , _hyperdataDocument_uniqId             :: Maybe Text
                                           , _hyperdataDocument_uniqIdBdd          :: Maybe Text
                                           , _hyperdataDocument_page               :: Maybe Int
                                           , _hyperdataDocument_title              :: Maybe Text
                                           , _hyperdataDocument_authors            :: Maybe Text
                                           , _hyperdataDocument_institutes         :: Maybe Text
                                           , _hyperdataDocument_source             :: Maybe Text
                                           , _hyperdataDocument_abstract           :: Maybe Text
                                           , _hyperdataDocument_publication_date   :: Maybe Text
                                           , _hyperdataDocument_publication_year   :: Maybe Int
                                           , _hyperdataDocument_publication_month  :: Maybe Int
                                           , _hyperdataDocument_publication_day    :: Maybe Int
                                           , _hyperdataDocument_publication_hour   :: Maybe Int
                                           , _hyperdataDocument_publication_minute :: Maybe Int
                                           , _hyperdataDocument_publication_second :: Maybe Int
                                           , _hyperdataDocument_language_iso2      :: Maybe Text
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "_hyperdataDocument_") ''HyperdataDocument)
$(makeLenses ''HyperdataDocument)

instance Eq HyperdataDocument where
  (==) h1 h2 = (==) (_hyperdataDocument_uniqId h1) (_hyperdataDocument_uniqId h2)

instance Ord HyperdataDocument where
  compare h1 h2 = compare (_hyperdataDocument_uniqId h1) (_hyperdataDocument_uniqId h2)

instance Hyperdata HyperdataDocument

instance ToField HyperdataDocument where
  toField = toJSONField

toHyperdataDocuments :: [(Text, Text)] -> [HyperdataDocument]
toHyperdataDocuments ts = map (\(t1,t2) -> HyperdataDocument Nothing Nothing Nothing Nothing Nothing Nothing (Just t1)
                                           Nothing Nothing (Just t2) Nothing Nothing Nothing Nothing Nothing 
                                           Nothing Nothing Nothing   Nothing
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

instance ToSchema EventLevel where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

------------------------------------------------------------------------

data Event = Event { event_level   :: EventLevel
                   , event_message :: Text
                   , event_date    :: UTCTime
            } deriving (Show, Generic)
$(deriveJSON (unPrefix "event_") ''Event)

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema Event where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

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

instance ToSchema Resource where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

------------------------------------------------------------------------
data HyperdataUser = HyperdataUser { hyperdataUser_language       :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataUser_") ''HyperdataUser)

instance Hyperdata HyperdataUser
------------------------------------------------------------------------
data HyperdataFolder = HyperdataFolder { hyperdataFolder_desc    :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFolder_") ''HyperdataFolder)

instance Hyperdata HyperdataFolder
------------------------------------------------------------------------
data HyperdataCorpus = HyperdataCorpus { hyperdataCorpus_title        :: Maybe Text
                                       , hyperdataCorpus_desc         :: Maybe Text
                                       , hyperdataCorpus_query        :: Maybe Text
                                       , hyperdataCorpus_authors      :: Maybe Text
                                       , hyperdataCorpus_resources    :: Maybe [Resource]
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataCorpus_") ''HyperdataCorpus)

instance Hyperdata HyperdataCorpus

corpusExample :: ByteString
corpusExample = "" -- TODO

defaultCorpus :: HyperdataCorpus
defaultCorpus = (HyperdataCorpus (Just "Title") (Just "Descr") (Just "Bool query") (Just "Authors") Nothing)

hyperdataCorpus :: HyperdataCorpus
hyperdataCorpus = case decode corpusExample of
  Just hp -> hp
  Nothing -> defaultCorpus

instance Arbitrary HyperdataCorpus where
    arbitrary = pure hyperdataCorpus -- TODO

------------------------------------------------------------------------
data HyperdataAnnuaire = HyperdataAnnuaire { hyperdataAnnuaire_title        :: Maybe Text
                                           , hyperdataAnnuaire_desc         :: Maybe Text
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataAnnuaire_") ''HyperdataAnnuaire)

instance Hyperdata HyperdataAnnuaire

hyperdataAnnuaire :: HyperdataAnnuaire
hyperdataAnnuaire = HyperdataAnnuaire (Just "Annuaire Title") (Just "Annuaire Description")

instance Arbitrary HyperdataAnnuaire where
    arbitrary = pure hyperdataAnnuaire -- TODO

------------------------------------------------------------------------
newtype HyperdataAny = HyperdataAny Object
  deriving (Show, Generic, ToJSON, FromJSON)

instance Hyperdata HyperdataAny

instance Arbitrary HyperdataAny where
    arbitrary = pure $ HyperdataAny mempty -- TODO produce arbitrary objects
------------------------------------------------------------------------

data HyperdataList = HyperdataList { hyperdataList_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataList_") ''HyperdataList)

instance Hyperdata HyperdataList
------------------------------------------------------------------------
data HyperdataScore = HyperdataScore { hyperdataScore_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataScore_") ''HyperdataScore)

instance Hyperdata HyperdataScore

------------------------------------------------------------------------

data HyperdataResource = HyperdataResource { hyperdataResource_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataResource_") ''HyperdataResource)

instance Hyperdata HyperdataResource

------------------------------------------------------------------------
data HyperdataDashboard = HyperdataDashboard { hyperdataDashboard_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDashboard_") ''HyperdataDashboard)

instance Hyperdata HyperdataDashboard

-- TODO add the Graph Structure here
data HyperdataGraph = HyperdataGraph { hyperdataGraph_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataGraph_") ''HyperdataGraph)

instance Hyperdata HyperdataGraph
------------------------------------------------------------------------

-- TODO add the Graph Structure here
data HyperdataPhylo = HyperdataPhylo { hyperdataPhylo_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataPhylo_") ''HyperdataPhylo)

instance Hyperdata HyperdataPhylo

------------------------------------------------------------------------
-- | TODO FEATURE: Notebook saved in the node
data HyperdataNotebook = HyperdataNotebook { hyperdataNotebook_preferences   :: Maybe Text
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataNotebook_") ''HyperdataNotebook)

instance Hyperdata HyperdataNotebook


-- | NodePoly indicates that Node has a Polymorphism Type
type Node json   = NodePoly NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime json

-- type Node json   = NodePoly NodeId NodeTypeId UserId ParentId NodeName UTCTime json
type NodeTypeId   = Int
type NodeParentId = Int
type NodeUserId   = Int
type NodeName     = Text
type TSVector     = Text


-- | Then a Node can be either a Folder or a Corpus or a Document
type NodeUser     = Node HyperdataUser
type NodeFolder   = Node HyperdataFolder

type NodeCorpus   = Node HyperdataCorpus
type NodeCorpusV3 = Node HyperdataCorpus
type NodeDocument = Node HyperdataDocument

type NodeAnnuaire = Node HyperdataAnnuaire

-- | Any others nodes
type NodeAny      = Node HyperdataAny

---- | Then a Node can be either a Graph or a Phylo or a Notebook
type NodeList     = Node HyperdataList
type NodeGraph    = Node HyperdataGraph
type NodePhylo    = Node HyperdataPhylo
type NodeNotebook = Node HyperdataNotebook
------------------------------------------------------------------------
data NodeType = NodeUser 
              | NodeFolder
              | NodeCorpus     | NodeCorpusV3 | NodeDocument
              | NodeAnnuaire   | NodeContact
              -- | NodeOccurrences
              | NodeGraph
              | NodeDashboard  | NodeChart
              -- | Classification
              | NodeList
              -- | Metrics
              deriving (Show, Read, Eq, Generic, Bounded, Enum)

allNodeTypes :: [NodeType]
allNodeTypes = [minBound ..]

instance FromJSON NodeType
instance ToJSON NodeType

instance FromHttpApiData NodeType 
  where 
      parseUrlPiece = Right . read . unpack

instance ToParamSchema NodeType
instance ToSchema      NodeType

------------------------------------------------------------------------
data NodePoly id        typename userId 
              parentId  name     date 
              hyperdata  = Node { _node_id        :: id
                                , _node_typename  :: typename
                                
                                , _node_userId    :: userId
                                , _node_parentId  :: parentId
                                
                                , _node_name      :: name
                                , _node_date      :: date
                                
                                , _node_hyperdata :: hyperdata
                                } deriving (Show, Generic)
$(deriveJSON (unPrefix "_node_") ''NodePoly)
$(makeLenses ''NodePoly)


data NodePolySearch id        typename userId 
              parentId  name     date 
              hyperdata search = NodeSearch { _ns_id        :: id
                                      , _ns_typename  :: typename
                                      , _ns_userId    :: userId
                                                                --   , nodeUniqId    :: hashId
                                      , _ns_parentId  :: parentId
                                      , _ns_name      :: name
                                      , _ns_date      :: date
                                  
                                      , _ns_hyperdata :: hyperdata
                                      , _ns_search    :: search
                                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "_ns_") ''NodePolySearch)
$(makeLenses ''NodePolySearch)

type NodeSearch json   = NodePolySearch NodeId NodeTypeId NodeUserId (Maybe NodeParentId) NodeName UTCTime json (Maybe TSVector)
------------------------------------------------------------------------


instance (Arbitrary hyperdata
         ,Arbitrary nodeId
         ,Arbitrary nodeTypeId
         ,Arbitrary nodeUserId
         ,Arbitrary nodeParentId
         ) => Arbitrary (NodePoly nodeId nodeTypeId nodeUserId nodeParentId
                                  NodeName UTCTime hyperdata) where
    --arbitrary = Node 1 1 (Just 1) 1 "name" (jour 2018 01 01) (arbitrary) (Just "")
    arbitrary = Node <$> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary

instance (Arbitrary hyperdata
         ,Arbitrary nodeId
         ,Arbitrary nodeTypeId
         ,Arbitrary nodeUserId
         ,Arbitrary nodeParentId
         ) => Arbitrary (NodePolySearch nodeId nodeTypeId nodeUserId nodeParentId
                                  NodeName UTCTime hyperdata (Maybe TSVector)) where
    --arbitrary = Node 1 1 (Just 1) 1 "name" (jour 2018 01 01) (arbitrary) (Just "")
    arbitrary = NodeSearch <$> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary


------------------------------------------------------------------------
hyperdataDocument :: HyperdataDocument
hyperdataDocument = case decode docExample of
                      Just hp -> hp
                      Nothing -> HyperdataDocument Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing
docExample :: ByteString
docExample = "{\"doi\":\"sdfds\",\"publication_day\":6,\"language_iso2\":\"en\",\"publication_minute\":0,\"publication_month\":7,\"language_iso3\":\"eng\",\"publication_second\":0,\"authors\":\"Nils Hovdenak, Kjell Haram\",\"publication_year\":2012,\"publication_date\":\"2012-07-06 00:00:00+00:00\",\"language_name\":\"English\",\"realdate_full_\":\"2012 01 12\",\"source\":\"European journal of obstetrics, gynecology, and reproductive biology\",\"abstract\":\"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome.\",\"title\":\"Influence of mineral and vitamin supplements on pregnancy outcome.\",\"publication_hour\":0}"

instance ToSchema HyperdataCorpus where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    L.& mapped.schema.description ?~ "a corpus"
    L.& mapped.schema.example ?~ toJSON hyperdataCorpus


instance ToSchema HyperdataAnnuaire where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    L.& mapped.schema.description ?~ "an annuaire"
    L.& mapped.schema.example ?~ toJSON hyperdataAnnuaire


instance ToSchema HyperdataDocument where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    L.& mapped.schema.description ?~ "a document"
    L.& mapped.schema.example ?~ toJSON hyperdataDocument


instance ToSchema HyperdataAny where
  declareNamedSchema proxy =
    pure $ genericNameSchema defaultSchemaOptions proxy mempty
             L.& schema.description ?~ "a node"
             L.& schema.example ?~ emptyObject -- TODO


instance ToSchema hyperdata =>
         ToSchema (NodePoly NodeId NodeTypeId
                            (Maybe NodeUserId)
                            NodeParentId NodeName
                            UTCTime hyperdata
                  )

instance ToSchema hyperdata =>
         ToSchema (NodePoly NodeId NodeTypeId
                            NodeUserId
                            (Maybe NodeParentId) NodeName
                            UTCTime hyperdata
                  )


instance ToSchema hyperdata =>
         ToSchema (NodePolySearch NodeId NodeTypeId
                            (Maybe NodeUserId)
                            NodeParentId NodeName
                            UTCTime hyperdata (Maybe TSVector)
                  )

instance ToSchema hyperdata =>
         ToSchema (NodePolySearch NodeId NodeTypeId
                            NodeUserId
                            (Maybe NodeParentId) NodeName
                            UTCTime hyperdata (Maybe TSVector)
                  )


instance ToSchema Status


