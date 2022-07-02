{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Annuaire where

import Control.Lens
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  , lift
  )
import Data.Proxy
import Data.Text (Text)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Types.Hyperdata.Contact
  ( HyperdataContact
  , ContactWho
  , cw_firstName
  , cw_lastName
  , hc_who, ContactWhere, hc_where, cw_organization, cw_labTeamDepts, cw_role, cw_office, cw_country, cw_city, cw_touch, ct_mail, ct_phone, ct_url, hc_title, hc_source)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Context (getContextWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import GHC.Generics (Generic)

data AnnuaireContact = AnnuaireContact
  { ac_title        :: Maybe Text
  , ac_source       :: Maybe Text
  , ac_id           :: Int
  , ac_firstName    :: Maybe Text
  , ac_lastName     :: Maybe Text
  , ac_labTeamDepts :: [Text]
  , ac_organization :: [Text]
  , ac_role         :: Maybe Text
  , ac_office       :: Maybe Text
  , ac_country      :: Maybe Text
  , ac_city         :: Maybe Text
  , ac_touchMail    :: Maybe Text
  , ac_touchPhone   :: Maybe Text
  , ac_touchUrl     :: Maybe Text
  }
  deriving (Generic, GQLType, Show)

-- | Arguments to the "user info" query.
data AnnuaireContactArgs
  = AnnuaireContactArgs
    { contact_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve user from a query.
resolveAnnuaireContacts
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => AnnuaireContactArgs -> GqlM e env [AnnuaireContact]
resolveAnnuaireContacts AnnuaireContactArgs { contact_id } = dbAnnuaireContacts contact_id

-- | Inner function to fetch the user from DB.
dbAnnuaireContacts
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> GqlM e env [AnnuaireContact]
dbAnnuaireContacts contact_id = do
  -- lift $ printDebug "[dbUsers]" user_id
--  user <- getUsersWithId user_id
--  hyperdata <- getUserHyperdata user_id
--  lift (map toUser <$> zip user hyperdata)
  c <- lift $ getContextWith (NodeId contact_id) (Proxy :: Proxy HyperdataContact)
  pure [toAnnuaireContact (contact_id, c ^. node_hyperdata)]

toAnnuaireContact :: (Int, HyperdataContact) -> AnnuaireContact
toAnnuaireContact (c_id, c_hyperdata) =
  AnnuaireContact { ac_title = c_hyperdata ^. ac_titleL
                  , ac_source = c_hyperdata ^. ac_sourceL
                  , ac_id = c_id
                  , ac_firstName = c_hyperdata ^. ac_firstNameL
                  , ac_lastName = c_hyperdata ^. ac_lastNameL
                  , ac_organization = c_hyperdata ^. ac_organizationL
                  , ac_labTeamDepts = c_hyperdata ^. ac_labTeamDeptsL
                  , ac_role = c_hyperdata ^. ac_roleL
                  , ac_office = c_hyperdata ^. ac_officeL
                  , ac_country = c_hyperdata ^. ac_countryL
                  , ac_city = c_hyperdata ^. ac_cityL
                  , ac_touchMail = c_hyperdata ^. ac_touchMailL
                  , ac_touchPhone = c_hyperdata ^. ac_touchPhoneL
                  , ac_touchUrl = c_hyperdata ^. ac_touchUrlL }

ac_titleL :: Traversal' HyperdataContact (Maybe Text)
ac_titleL = hc_title
ac_sourceL :: Traversal' HyperdataContact (Maybe Text)
ac_sourceL = hc_source
contactWhoL :: Traversal' HyperdataContact ContactWho
contactWhoL = hc_who . _Just
ac_firstNameL :: Traversal' HyperdataContact (Maybe Text)
ac_firstNameL = contactWhoL . cw_firstName
ac_lastNameL :: Traversal' HyperdataContact (Maybe Text)
ac_lastNameL = contactWhoL . cw_lastName
contactWhereL :: Traversal' HyperdataContact ContactWhere
contactWhereL = hc_where . ix 0
ac_organizationL :: Traversal' HyperdataContact [Text]
ac_organizationL = contactWhereL . cw_organization
ac_labTeamDeptsL :: Traversal' HyperdataContact [Text]
ac_labTeamDeptsL = contactWhereL . cw_labTeamDepts
ac_roleL :: Traversal' HyperdataContact (Maybe Text)
ac_roleL = contactWhereL . cw_role
ac_officeL :: Traversal' HyperdataContact (Maybe Text)
ac_officeL = contactWhereL . cw_office
ac_countryL :: Traversal' HyperdataContact (Maybe Text)
ac_countryL = contactWhereL . cw_country
ac_cityL :: Traversal' HyperdataContact (Maybe Text)
ac_cityL = contactWhereL . cw_city
ac_touchMailL :: Traversal' HyperdataContact (Maybe Text)
ac_touchMailL = contactWhereL . cw_touch . _Just . ct_mail
ac_touchPhoneL :: Traversal' HyperdataContact (Maybe Text)
ac_touchPhoneL = contactWhereL . cw_touch . _Just . ct_phone
ac_touchUrlL :: Traversal' HyperdataContact (Maybe Text)
ac_touchUrlL = contactWhereL . cw_touch . _Just . ct_url
