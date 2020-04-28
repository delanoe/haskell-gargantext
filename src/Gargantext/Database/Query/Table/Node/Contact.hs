{-|
Module      : Gargantext.Database.Query.Table.Node.Contact
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.Node.Contact
  where

import Control.Lens (makeLenses)
import Data.Time.Segment (jour)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Node (Hyperdata, Node)
import Gargantext.Database.Admin.Utils (fromField')
import Gargantext.Prelude
import Opaleye (QueryRunnerColumnDefault, queryRunnerColumnDefault, PGJsonb, fieldQueryRunnerColumn)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

type NodeContact  = Node HyperdataContact

data HyperdataContact =
     HyperdataContact { _hc_bdd    :: Maybe Text           -- ID of Database source
                      , _hc_who    :: Maybe ContactWho
                      , _hc_where  :: [ContactWhere]
                      , _hc_title  :: Maybe Text -- TODO remove (only demo)
                      , _hc_source :: Maybe Text -- TODO remove (only demo)
                      , _hc_lastValidation  :: Maybe Text -- TODO UTCTime
                      , _hc_uniqIdBdd       :: Maybe Text
                      , _hc_uniqId          :: Maybe Text

  } deriving (Eq, Show, Generic)


fake_HyperdataContact :: HyperdataContact
fake_HyperdataContact = HyperdataContact (Just "bdd")
                                         (Just fake_ContactWho)
                                         [fake_ContactWhere]
                                         (Just "Title")
                                         (Just "Source")
                                         (Just "TODO lastValidation date")
                                         (Just "DO NOT expose this")
                                         (Just "DO NOT expose this")


-- TOD0 contact metadata (Type is too flat)
data ContactMetaData =
     ContactMetaData { _cm_bdd :: Maybe Text
                     , _cm_lastValidation  :: Maybe Text -- TODO UTCTIME
  } deriving (Eq, Show, Generic)

fake_ContactMetaData :: ContactMetaData
fake_ContactMetaData = ContactMetaData (Just "bdd") (Just "TODO UTCTime")

arbitraryHyperdataContact :: HyperdataContact
arbitraryHyperdataContact = HyperdataContact Nothing Nothing []
                                             Nothing Nothing Nothing
                                             Nothing Nothing


data ContactWho = 
     ContactWho { _cw_id          :: Maybe Text
                , _cw_firstName   :: Maybe Text
                , _cw_lastName    :: Maybe Text
                , _cw_keywords :: [Text]
                , _cw_freetags :: [Text]
  } deriving (Eq, Show, Generic)

fake_ContactWho :: ContactWho
fake_ContactWho = ContactWho (Just "123123")
                             (Just "First Name")
                             (Just "Last Name")
                             ["keyword A"]
                             ["freetag A"]

data ContactWhere =
     ContactWhere { _cw_organization :: [Text]
                  , _cw_labTeamDepts :: [Text]

                  , _cw_role         :: Maybe Text

                  , _cw_office       :: Maybe Text
                  , _cw_country      :: Maybe Text
                  , _cw_city         :: Maybe Text

                  , _cw_touch        :: Maybe ContactTouch

                  , _cw_entry        :: Maybe UTCTime
                  , _cw_exit         :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

fake_ContactWhere :: ContactWhere
fake_ContactWhere = ContactWhere ["Organization A"]
                                 ["Organization B"]
                                 (Just "Role")
                                 (Just "Office")
                                 (Just "Country")
                                 (Just "City")
                                 (Just fake_ContactTouch)
                                 (Just $ jour 01 01 2020)
                                 (Just $ jour 01 01 2029)

data ContactTouch =
     ContactTouch { _ct_mail      :: Maybe Text
                  , _ct_phone     :: Maybe Text
                  , _ct_url       :: Maybe Text
  } deriving (Eq, Show, Generic)

fake_ContactTouch :: ContactTouch
fake_ContactTouch = ContactTouch (Just "email@data.com")
                                 (Just "+336 328 283 288")
                                 (Just "https://url.com")

-- | ToSchema instances
instance ToSchema HyperdataContact where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hc_")
instance ToSchema ContactWho where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_cw_")
instance ToSchema ContactWhere where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_cw_")
instance ToSchema ContactTouch where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ct_")
instance ToSchema ContactMetaData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_cm_")

-- | Arbitrary instances
instance Arbitrary HyperdataContact where
  arbitrary = elements [HyperdataContact Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing]

-- | Specific Gargantext instance
instance Hyperdata HyperdataContact

-- | Database (Posgresql-simple instance)
instance FromField HyperdataContact where
  fromField = fromField'

-- | Database (Opaleye instance)
instance QueryRunnerColumnDefault PGJsonb HyperdataContact   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- | All lenses
makeLenses ''ContactWho
makeLenses ''ContactWhere
makeLenses ''ContactTouch
makeLenses ''ContactMetaData
makeLenses ''HyperdataContact

-- | All Json instances
$(deriveJSON (unPrefix "_cw_") ''ContactWho)
$(deriveJSON (unPrefix "_cw_") ''ContactWhere)
$(deriveJSON (unPrefix "_ct_") ''ContactTouch)
$(deriveJSON (unPrefix "_cm_") ''ContactMetaData)
$(deriveJSON (unPrefix "_hc_") ''HyperdataContact)
