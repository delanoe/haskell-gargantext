{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Contact
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}


module Gargantext.Database.Admin.Types.Hyperdata.Contact
  where

import Data.Morpheus.Types (GQLType(..))
import Data.Time.Segment (jour)
import qualified Gargantext.API.GraphQL.Utils as GAGU
import Gargantext.Core.Text (HasText(..))
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Prelude
import Gargantext.Utils.UTCTime

--------------------------------------------------------------------------------
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

instance GQLType HyperdataContact where
  typeOptions _ = GAGU.unPrefix "_hc_"

instance HasText HyperdataContact
  where
    hasText = undefined

defaultHyperdataContact :: HyperdataContact
defaultHyperdataContact =
  HyperdataContact
    { _hc_bdd = Just "bdd"
    , _hc_who = Just defaultContactWho
    , _hc_where = [defaultContactWhere]
    , _hc_title =Just "Title"
    , _hc_source = Just "Source"
    , _hc_lastValidation = Just "TODO lastValidation date"
    , _hc_uniqIdBdd = Just "DO NOT expose this"
    , _hc_uniqId = Just "DO NOT expose this" }

hyperdataContact :: FirstName -> LastName -> HyperdataContact
hyperdataContact fn ln =
  HyperdataContact
    { _hc_bdd = Nothing
    , _hc_who = Just (contactWho fn ln)
    , _hc_where = []
    , _hc_title = Nothing
    , _hc_source = Nothing
    , _hc_lastValidation = Nothing
    , _hc_uniqIdBdd = Nothing
    , _hc_uniqId = Nothing }

-- TOD0 contact metadata (Type is too flat)
data ContactMetaData =
  ContactMetaData { _cm_bdd :: Maybe Text
                  , _cm_lastValidation  :: Maybe Text -- TODO UTCTIME
  } deriving (Eq, Show, Generic)

defaultContactMetaData :: ContactMetaData
defaultContactMetaData = ContactMetaData (Just "bdd") (Just "TODO UTCTime")

arbitraryHyperdataContact :: HyperdataContact
arbitraryHyperdataContact =
  HyperdataContact
    { _hc_bdd = Nothing
    , _hc_who = Nothing
    , _hc_where = []
    , _hc_title = Nothing
    , _hc_source = Nothing
    , _hc_lastValidation = Nothing
    , _hc_uniqIdBdd = Nothing
    , _hc_uniqId = Nothing }


data ContactWho = 
  ContactWho { _cw_id          :: Maybe Text
             , _cw_firstName   :: Maybe Text
             , _cw_lastName    :: Maybe Text
             , _cw_keywords :: [Text]
             , _cw_freetags :: [Text]
             , _cw_description :: Maybe Text
  } deriving (Eq, Show, Generic)

instance GQLType ContactWho where
  typeOptions _ = GAGU.unPrefix "_cw_"

type FirstName = Text
type LastName  = Text

defaultContactWho :: ContactWho
defaultContactWho = contactWho "Pierre" "Dupont"

contactWho :: FirstName -> LastName -> ContactWho
contactWho fn ln =
  ContactWho { _cw_id = Nothing
             , _cw_firstName = Just fn
             , _cw_lastName = Just ln
             , _cw_keywords = []
             , _cw_freetags = []
             , _cw_description = Nothing }

data ContactWhere =
  ContactWhere { _cw_organization :: [Text]
               , _cw_labTeamDepts :: [Text]
               
               , _cw_role         :: Maybe Text
               
               , _cw_office       :: Maybe Text
               , _cw_country      :: Maybe Text
               , _cw_city         :: Maybe Text
               
               , _cw_touch        :: Maybe ContactTouch
               
               , _cw_entry        :: Maybe NUTCTime
               , _cw_exit         :: Maybe NUTCTime
  } deriving (Eq, Show, Generic)

instance GQLType ContactWhere where
  typeOptions _ = GAGU.unPrefix "_cw_"

defaultContactWhere :: ContactWhere
defaultContactWhere =
  ContactWhere
    { _cw_organization = ["Organization X"]
    , _cw_labTeamDepts = ["Lab Z"]
    , _cw_role = Just "Role"
    , _cw_office = Just "Office"
    , _cw_country = Just "Country"
    , _cw_city = Just "City"
    , _cw_touch = Just defaultContactTouch
    , _cw_entry = Just $ NUTCTime $ jour 01 01 2020
    , _cw_exit = Just $ NUTCTime $ jour 01 01 2029 }

data ContactTouch =
     ContactTouch { _ct_mail      :: Maybe Text
                  , _ct_phone     :: Maybe Text
                  , _ct_url       :: Maybe Text
  } deriving (Eq, Show, Generic)

instance GQLType ContactTouch where
  typeOptions _ = GAGU.unPrefix "_ct_"

defaultContactTouch :: ContactTouch
defaultContactTouch =
  ContactTouch
    { _ct_mail = Just "email@data.com"
    , _ct_phone = Just "+336 328 283 288"
    , _ct_url = Just "https://url.com" }

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
instance DefaultFromField SqlJsonb HyperdataContact   where
  defaultFromField = fromPGSFromField


instance DefaultFromField (Nullable SqlJsonb) HyperdataContact where
  defaultFromField = fromPGSFromField



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
