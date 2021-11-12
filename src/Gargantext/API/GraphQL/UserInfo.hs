{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.UserInfo where

import Control.Lens
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , ResolverM
  , QUERY
  , lift
  )
import Data.Text (Text)
import qualified Data.Text as T
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Admin.Types.Hyperdata
  ( HyperdataUser(..)
  , hc_source
  , hc_title
  , hu_shared)
import Gargantext.Database.Admin.Types.Hyperdata.Contact
  ( HyperdataContact
  , ContactWho
  , ContactWhere
  , cw_city
  , cw_country
  , cw_firstName
  , cw_lastName
  , cw_labTeamDepts
  , cw_office
  , cw_organization
  , cw_role
  , cw_touch
  , ct_mail
  , ct_phone
  , hc_who
  , hc_where)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.User (getUsersWithHyperdata)
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import GHC.Generics (Generic)

data UserInfo = UserInfo
  { ui_id             :: Int
  , ui_username       :: Text
  , ui_email          :: Text
  , ui_title          :: Maybe Text
  , ui_source         :: Maybe Text
  , ui_cwFirstName    :: Maybe Text
  , ui_cwLastName     :: Maybe Text
  , ui_cwCity         :: Maybe Text
  , ui_cwCountry      :: Maybe Text
  , ui_cwOrganization :: [Text]
  , ui_cwLabTeamDepts :: [Text]
  , ui_cwOffice       :: Maybe Text
  , ui_cwRole         :: Maybe Text
  , ui_cwTouchPhone   :: Maybe Text
  , ui_cwTouchMail    :: Maybe Text }
  deriving (Generic, GQLType, Show)

-- | Arguments to the "user info" query.
data UserInfoArgs
  = UserInfoArgs
    { user_id :: Int
    } deriving (Generic, GQLType)

-- | Arguments to the "user info" mutation,
data UserInfoMArgs
  = UserInfoMArgs
    { ui_id :: Int
    , ui_username       :: Maybe Text
    , ui_email          :: Maybe Text
    , ui_title          :: Maybe Text
    , ui_source         :: Maybe Text
    , ui_cwFirstName    :: Maybe Text
    , ui_cwLastName     :: Maybe Text
    , ui_cwCity         :: Maybe Text
    , ui_cwCountry      :: Maybe Text
    , ui_cwOrganization :: Maybe [Text]
    , ui_cwLabTeamDepts :: Maybe [Text]
    , ui_cwOffice       :: Maybe Text
    , ui_cwRole         :: Maybe Text
    , ui_cwTouchPhone   :: Maybe Text
    , ui_cwTouchMail    :: Maybe Text 
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve user from a query.
resolveUserInfos
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => UserInfoArgs -> GqlM e env [UserInfo]
resolveUserInfos UserInfoArgs { user_id } = dbUsers user_id

-- | Mutation for user info
updateUserInfo
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => UserInfoMArgs -> ResolverM e (GargM env GargError) Int
updateUserInfo (UserInfoMArgs { ui_id, .. }) = do
  lift $ printDebug "[updateUserInfo] ui_id" ui_id
  users <- lift (getUsersWithHyperdata ui_id)
  case users of
    [] -> panic $ "[updateUserInfo] User with id " <> (T.pack $ show ui_id) <> " doesn't exist."
    ((u, u_hyperdata):_) -> do
      lift $ printDebug "[updateUserInfo] u" u
      let u_hyperdata' = uh ui_titleL ui_title $
                         uh ui_sourceL ui_source $
                         uh ui_cwFirstNameL ui_cwFirstName $
                         uh ui_cwLastNameL ui_cwLastName $
                         uh ui_cwCityL ui_cwCity $
                         uh ui_cwCountryL ui_cwCountry $
                         uh' ui_cwLabTeamDeptsL ui_cwLabTeamDepts $
                         uh' ui_cwOrganizationL ui_cwOrganization $
                         uh ui_cwOfficeL ui_cwOffice $
                         uh ui_cwRoleL ui_cwRole $
                         uh ui_cwTouchMailL ui_cwTouchMail $
                         uh ui_cwTouchPhoneL ui_cwTouchPhone $
                         u_hyperdata
      lift $ printDebug "[updateUserInfo] with firstName" u_hyperdata'
      _ <- lift $ updateHyperdata (NodeId ui_id) u_hyperdata'
      --let _newUser = toUser (u, u_hyperdata')
      pure 1
  where
    uh _ Nothing u_hyperdata = u_hyperdata
    uh lens' (Just val) u_hyperdata = u_hyperdata & lens' .~ Just val
    uh' _ Nothing u_hyperdata = u_hyperdata
    uh' lens' (Just val) u_hyperdata = u_hyperdata & lens' .~ val

-- | Inner function to fetch the user from DB.
dbUsers
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> GqlM e env [UserInfo]
dbUsers user_id = do
--  user <- getUsersWithId user_id
--  hyperdata <- getUserHyperdata user_id
--  lift (map toUser <$> zip user hyperdata)
  lift (map toUser <$> (getUsersWithHyperdata user_id))

toUser :: (UserLight, HyperdataUser) -> UserInfo
toUser (UserLight { .. }, u_hyperdata) =
  UserInfo { ui_id             = userLight_id
           , ui_username       = userLight_username
           , ui_email          = userLight_email
           , ui_title          = u_hyperdata ^. ui_titleL
           , ui_source         = u_hyperdata ^. ui_sourceL
           , ui_cwFirstName    = u_hyperdata ^. ui_cwFirstNameL
           , ui_cwLastName     = u_hyperdata ^. ui_cwLastNameL
           , ui_cwCity         = u_hyperdata ^. ui_cwCityL
           , ui_cwCountry      = u_hyperdata ^. ui_cwCountryL
           , ui_cwLabTeamDepts = u_hyperdata ^. ui_cwLabTeamDeptsL
           , ui_cwOrganization = u_hyperdata ^. ui_cwOrganizationL
           , ui_cwOffice       = u_hyperdata ^. ui_cwOfficeL
           , ui_cwRole         = u_hyperdata ^. ui_cwRoleL
           , ui_cwTouchMail    = u_hyperdata ^. ui_cwTouchMailL
           , ui_cwTouchPhone   = u_hyperdata ^. ui_cwTouchPhoneL }

sharedL :: Traversal' HyperdataUser HyperdataContact
sharedL = hu_shared . _Just
ui_titleL :: Traversal' HyperdataUser (Maybe Text)
ui_titleL = sharedL . hc_title
ui_sourceL :: Traversal' HyperdataUser (Maybe Text)
ui_sourceL = sharedL . hc_source
contactWhoL :: Traversal' HyperdataUser ContactWho
contactWhoL = sharedL . hc_who . _Just
ui_cwFirstNameL :: Traversal' HyperdataUser (Maybe Text)
ui_cwFirstNameL = contactWhoL . cw_firstName
ui_cwLastNameL :: Traversal' HyperdataUser (Maybe Text)
ui_cwLastNameL = contactWhoL . cw_lastName
contactWhereL :: Traversal' HyperdataUser ContactWhere
contactWhereL = sharedL . hc_where . (ix 0)
ui_cwCityL :: Traversal' HyperdataUser (Maybe Text)
ui_cwCityL = contactWhereL . cw_city
ui_cwCountryL :: Traversal' HyperdataUser (Maybe Text)
ui_cwCountryL = contactWhereL . cw_country
ui_cwLabTeamDeptsL :: Traversal' HyperdataUser [Text]
ui_cwLabTeamDeptsL = hu_shared . _Just . (hc_where . (ix 0) . cw_labTeamDepts)
ui_cwOrganizationL :: Traversal' HyperdataUser [Text]
ui_cwOrganizationL = hu_shared . _Just . (hc_where . (ix 0) . cw_organization)
ui_cwOfficeL :: Traversal' HyperdataUser (Maybe Text)
ui_cwOfficeL = contactWhereL . cw_office
ui_cwRoleL :: Traversal' HyperdataUser (Maybe Text)
ui_cwRoleL = contactWhereL . cw_role
ui_cwTouchMailL :: Traversal' HyperdataUser (Maybe Text)
ui_cwTouchMailL = hu_shared . _Just . (hc_where . (ix 0) . cw_touch . _Just . ct_mail)
--ui_cwTouchMailL = contactWhereL . cw_touch . _Just . ct_mail
ui_cwTouchPhoneL :: Traversal' HyperdataUser (Maybe Text)
ui_cwTouchPhoneL = hu_shared . _Just . (hc_where . (ix 0) . cw_touch . _Just . ct_phone)
--ui_cwTouchPhoneL = contactWhereL . cw_touch . _Just . ct_phone
