{-# LANGUAGE DeriveAnyClass #-}

module Gargantext.API.GraphQL.User where

import Data.Maybe (listToMaybe)
import Data.Morpheus.Types
  ( GQLType
  , Resolver, QUERY
  , lift
  )
import Data.Text (Text)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.User (getUsersWithId, getUserHyperdata)
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import GHC.Generics (Generic)

data User m = User
  { u_email     :: Text
  , u_hyperdata :: m (Maybe HyperdataUser)
  , u_id        :: Int
  , u_username  :: Text }
  deriving (Generic, GQLType)

-- | Arguments to the "user" query.
data UserArgs
  = UserArgs
    { user_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve user from a query.
resolveUsers
  :: (HasConnectionPool env, HasConfig env)
  => UserArgs -> GqlM e env [User (GqlM e env)]
resolveUsers UserArgs { user_id } = dbUsers user_id

-- | Inner function to fetch the user from DB.
dbUsers
  :: (HasConnectionPool env, HasConfig env)
  => Int -> GqlM e env ([User (GqlM e env)])
dbUsers user_id = lift (map toUser <$> getUsersWithId user_id)

toUser
  :: (HasConnectionPool env, HasConfig env)
  => UserLight -> User (GqlM e env)
toUser (UserLight { .. }) = User { u_email = userLight_email
                                 , u_hyperdata = resolveHyperdata userLight_id
                                 , u_id = userLight_id
                                 , u_username = userLight_username }

resolveHyperdata
  :: (HasConnectionPool env, HasConfig env)
  => Int -> GqlM e env (Maybe HyperdataUser)
resolveHyperdata userid = lift (listToMaybe <$> getUserHyperdata userid)
