{-|
Module      : Gargantext.Database.Node.Contact
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

module Gargantext.Database.Node.Contact
  where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Core.Types (Name)
import Gargantext.Database.Schema.Node (NodeWrite, node)
import Gargantext.Database.Types.Node (Node,Hyperdata,NodeType(..), UserId, AnnuaireId)
import Gargantext.Database.Utils (fromField')
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
                      , _hc_lastValidation  :: Maybe Text
                      , _hc_uniqIdBdd       :: Maybe Text
                      , _hc_uniqId          :: Maybe Text

  } deriving (Eq, Show, Generic)

-- TOD0 contact metadata (Type is too flat)
data ContactMetaData =
     ContactMetaData { _cm_bdd :: Maybe Text
                     , _cm_lastValidation  :: Maybe Text
  } deriving (Eq, Show, Generic)


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

data ContactTouch =
     ContactTouch { _ct_mail      :: Maybe Text
                  , _ct_phone     :: Maybe Text
                  , _ct_url       :: Maybe Text
  } deriving (Eq, Show, Generic)


nodeContactW :: Maybe Name -> Maybe HyperdataContact
             -> AnnuaireId -> UserId -> NodeWrite
nodeContactW maybeName maybeContact aId = 
  node NodeContact name contact (Just aId)
    where
      name    = maybe "Contact" identity maybeName
      contact = maybe arbitraryHyperdataContact identity maybeContact


-- | Main instances of Contact
instance ToSchema HyperdataContact
instance ToSchema ContactWho
instance ToSchema ContactWhere
instance ToSchema ContactTouch

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
