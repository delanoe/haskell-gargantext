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
  , hc_who)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Context (getContextWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import GHC.Generics (Generic)

data AnnuaireContact = AnnuaireContact
  { ac_id        :: Int
  , ac_firstName :: Maybe Text
  , ac_lastName  :: Maybe Text
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
  AnnuaireContact { ac_id = c_id
                  , ac_firstName = c_hyperdata ^. ac_firstNameL
                  , ac_lastName = c_hyperdata ^. ac_lastNameL }

contactWhoL :: Traversal' HyperdataContact ContactWho
contactWhoL = hc_who . _Just
ac_firstNameL :: Traversal' HyperdataContact (Maybe Text)
ac_firstNameL = contactWhoL . cw_firstName
ac_lastNameL :: Traversal' HyperdataContact (Maybe Text)
ac_lastNameL = contactWhoL . cw_lastName

