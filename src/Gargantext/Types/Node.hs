{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Types.Node where

import Gargantext.Prelude

import Text.Show (Show())
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Gargantext.Utils.Prefix (unPrefix)
import Data.Aeson.TH (deriveJSON)



-- node_Id... ?
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


data Status  = Status { status_Date     :: Maybe UTCTime
                      , status_Error    :: Maybe Text
                      , status_Action   :: Maybe Text
                      , status_Complete :: Maybe Bool
                      , status_Progress :: Maybe Int
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "status_") ''Status)


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




