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

module Gargantext.Database.Node.Contact (NodeContact,HyperdataContact, ContactWho, ContactWhere, ContactTouch)
  where

import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as DT
import Control.Lens (makeLenses)
import Database.PostgreSQL.Simple
import Opaleye (QueryRunnerColumnDefault
               , queryRunnerColumnDefault, PGJsonb, fieldQueryRunnerColumn)
import Gargantext.Database.Utils (fromField')
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Node (NodeWrite', AnnuaireId, UserId, Name, node)
import Gargantext.Prelude
import Gargantext.Database.Types.Node (Node,Hyperdata,NodeType(..))
import Data.Aeson (Result(Error,Success), fromJSON, FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField ( Conversion
                                            , ResultError(ConversionFailed)
                                            , FromField
                                            , fromField
                                            , returnError
                                            )

------------------------------------------------------------------------

type NodeContact  = Node HyperdataContact

data HyperdataContact =
     HyperdataContact { _hc_who    :: Maybe ContactWho
                      , _hc_where  :: Maybe [ContactWhere]
                      , _hc_lastValidation  :: Maybe Text

  } deriving (Eq, Show, Generic)

arbitraryHyperdataContact :: HyperdataContact
arbitraryHyperdataContact = HyperdataContact Nothing Nothing Nothing

data ContactWho = 
     ContactWho { _cw_id          :: Maybe Int
                , _cw_firstName   :: Maybe Text
                , _cw_lastName    :: Maybe Text
                , _cw_keywords :: Maybe [Text]
                , _cw_freetags :: Maybe [Text]
  } deriving (Eq, Show, Generic)

data ContactWhere =
     ContactWhere { _cw_organization :: Maybe [Text]
                  , _cw_labTeamDepts :: Maybe [Text]
                  , _cw_role         :: Maybe Text
                  , _cw_office       :: Maybe Text
                  , _cw_country      :: Maybe Text
                  , _cw_city         :: Maybe Text
                  , _cw_touch        :: Maybe ContactTouch
  } deriving (Eq, Show, Generic)

data ContactTouch =
     ContactTouch { _ct_mail      :: Maybe Text
                  , _ct_phone     :: Maybe Text
                  , _ct_url       :: Maybe Text
  } deriving (Eq, Show, Generic)


nodeContactW :: Maybe Name -> Maybe HyperdataContact
             -> AnnuaireId -> UserId -> NodeWrite'
nodeContactW maybeName maybeContact aId = 
  node NodeContact name contact (Just aId)
    where
      name    = maybe "Contact" identity maybeName
      contact = maybe arbitraryHyperdataContact identity maybeContact



instance Hyperdata HyperdataContact
instance FromField HyperdataContact where
  fromField = fromField'
instance QueryRunnerColumnDefault PGJsonb HyperdataContact   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

{-
makeLenses ''ContactWho
makeLenses ''ContactWhere
makeLenses ''ContactTouch
makeLenses ''HyperdataContact
-}

$(deriveJSON (unPrefix "_cw_") ''ContactWho)
$(deriveJSON (unPrefix "_cw_") ''ContactWhere)
$(deriveJSON (unPrefix "_ct_") ''ContactTouch)
$(deriveJSON (unPrefix "_hc_") ''HyperdataContact)


