{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.UserInfo where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , ResolverM
  , QUERY
  , description
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
  , cw_description
  , ct_mail
  , ct_phone
  , hc_who
  , hc_where)
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.User (getUsersWithHyperdata, getUsersWithNodeHyperdata, updateUserEmail)
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Database.Schema.Node (node_id, node_hyperdata, NodePoly (Node, _node_id))
import Gargantext.Prelude
import GHC.Generics (Generic)
import Gargantext.API.GraphQL.Utils (AuthStatus(Invalid, Valid), authUser)
import Gargantext.API.Admin.Types (HasSettings)

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
  , ui_cwTouchMail    :: Maybe Text  -- TODO: Remove. userLight_email should be used instead
  , ui_cwDescription  :: Maybe Text
  }
  deriving (Generic, Show)
instance GQLType UserInfo where
  description = const $ Just "provides user info"

-- | Arguments to the "user info" query.
data UserInfoArgs
  = UserInfoArgs
    { user_id :: Int
    } deriving (Generic, GQLType)

-- | Arguments to the "user info" mutation,
data UserInfoMArgs
  = UserInfoMArgs
    { ui_id             :: Int
    , token             :: Text
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
    , ui_cwDescription  :: Maybe Text
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)
type GqlM' e env err = ResolverM e (GargM env err) Int

-- | Function to resolve user from a query.
resolveUserInfos
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => UserInfoArgs -> GqlM e env [UserInfo]
resolveUserInfos UserInfoArgs { user_id } = dbUsers user_id

-- | Mutation for user info
updateUserInfo
  :: (HasConnectionPool env, HasConfig env, HasMail env, HasSettings env)
  -- => UserInfoMArgs -> ResolverM e (GargM env err) Int
  => UserInfoMArgs -> GqlM' e env err
updateUserInfo (UserInfoMArgs { ui_id, .. }) = do
  -- lift $ printDebug "[updateUserInfo] ui_id" ui_id
  users <- lift (getUsersWithNodeHyperdata ui_id)
  case users of
    [] -> panic $ "[updateUserInfo] User with id " <> (T.pack $ show ui_id) <> " doesn't exist."
    ((UserLight { .. }, node_u):_) -> do
      testAuthUser <- lift $ authUser (nId node_u) token
      case testAuthUser of
        Invalid -> panic "[updateUserInfo] failed to validate user"
        Valid -> do
          let u_hyperdata = node_u ^. node_hyperdata
          -- lift $ printDebug "[updateUserInfo] u" u
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
                            uh ui_cwDescriptionL ui_cwDescription
                            u_hyperdata
          -- NOTE: We have 1 username and 2 emails: userLight_email and ui_cwTouchMail
          -- The userLight_email is more important: it is used for login and sending mail.
          -- Therefore we update ui_cwTouchMail and userLight_email.
          -- ui_cwTouchMail is to be removed in the future.
          let u' = UserLight { userLight_email = fromMaybe userLight_email $ view ui_cwTouchMailL u_hyperdata'
                             , .. }
          -- lift $ printDebug "[updateUserInfo] with firstName" u_hyperdata'
          _ <- lift $ updateHyperdata (node_u ^. node_id) u_hyperdata'
          _ <- lift $ updateUserEmail u'
          --let _newUser = toUser (u, u_hyperdata')
          pure 1
  where
    uh _ Nothing u_hyperdata = u_hyperdata
    uh lens' (Just val) u_hyperdata = u_hyperdata & lens' .~ Just val
    uh' _ Nothing u_hyperdata = u_hyperdata
    uh' lens' (Just val) u_hyperdata = u_hyperdata & lens' .~ val
    nId Node {_node_id} = _node_id

-- | Inner function to fetch the user from DB.
dbUsers
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => Int -> GqlM e env [UserInfo]
dbUsers user_id = do
  -- lift $ printDebug "[dbUsers]" user_id
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
           --, ui_cwTouchMail    = u_hyperdata ^. ui_cwTouchMailL
           , ui_cwTouchMail    = Just userLight_email
           , ui_cwTouchPhone   = u_hyperdata ^. ui_cwTouchPhoneL
           , ui_cwDescription  = u_hyperdata ^. ui_cwDescriptionL }

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
ui_cwDescriptionL :: Traversal' HyperdataUser (Maybe Text)
ui_cwDescriptionL = contactWhoL . cw_description
