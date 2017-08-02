{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Gargantext.Types.Node where

import Data.Text (Text)
import Data.List (lookup)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Gargantext.Utils.Prefix
import Data.Aeson.TH



data NodePoly id typename userId parentId name date hyperdata = Node { node_id        :: id
                                                                     , node_typename  :: typename
                                                                     , node_userId:: userId
                                                                --   , nodeHashId    :: hashId
                                                                     , node_parentId  :: parentId
                                                                     , node_name      :: name
                                                                     , node_date      :: date
                                                                     , node_hyperdata :: hyperdata
                                                                     } deriving (Show)


data Statut  = Statut { statut_Date     :: Maybe UTCTime
                      , statut_Error    :: Maybe Text
                      , statut_Action   :: Maybe Text
                      , statut_Complete :: Maybe Bool
                      , statut_Progress :: Maybe Int
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "statut_") ''Statut)


data HyperdataDocument = HyperdataDocument { hyperdataDocument_Bdd                :: Maybe Text
                                           , hyperdataDocument_Doi                :: Maybe Text
                                           , hyperdataDocument_Url                :: Maybe Text
                                           , hyperdataDocument_Page               :: Maybe Int
                                           , hyperdataDocument_Title              :: Maybe Text
                                           , hyperdataDocument_Authors            :: Maybe Text
                                           , hyperdataDocument_Abstract           :: Maybe Text
                                           , hyperdataDocument_Statuses           :: Maybe [Statut]
                                           , hyperdataDocument_Publication_date   :: Maybe Text
                                           , hyperdataDocument_Publication_year   :: Maybe Text
                                           , hyperdataDocument_Publication_month  :: Maybe Text
                                           , hyperdataDocument_Publication_hour   :: Maybe Text
                                           , hyperdataDocument_Publication_minute :: Maybe Text
                                           , hyperdataDocument_Publication_second :: Maybe Text
                                           , hyperdataDocument_LanguageIso2       :: Maybe Text
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDocument_") ''HyperdataDocument)

data LanguageNodes = LanguageNodes { languageNodes___unknown__ :: [Int]}
    deriving (Show, Generic)
$(deriveJSON (unPrefix "languageNodes_") ''LanguageNodes)


data Resource = Resource { resource_Url  :: Maybe Text
                         , resource_Path :: Maybe Text
                         , resource_Type :: Maybe Int
                         , resource_Extracted :: Maybe Bool
                         } deriving (Show, Generic)
$(deriveJSON (unPrefix "resource_") ''Resource)


data HyperdataCorpus = HyperdataCorpus { hyperdataCorpus_Action       :: Maybe Text
                                       , hyperdataCorpus_Statuses     :: Maybe [Statut]
                                       , hyperdataCorpus_Languages    :: Maybe LanguageNodes
                                       , hyperdataCorpus_Resources    :: Maybe [Resource]
                                       , hyperdataCorpus_Language_id  :: Maybe Text
                                       , hyperdataCorpus_Skipped_docs :: Maybe [Int]
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataCorpus_") ''HyperdataCorpus)


data HyperdataFolder = HyperdataFolder { hyperdataFolder_Preferences   :: Maybe Text
                                       } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataFolder_") ''HyperdataFolder)


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


