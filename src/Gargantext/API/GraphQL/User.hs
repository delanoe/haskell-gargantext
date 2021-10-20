{-# LANGUAGE DeriveAnyClass #-}

module Gargantext.API.GraphQL.User where

import Data.Either (Either(..))
import Data.Morpheus.Types
  ( GQLType
  , ResolverQ
  , liftEither
  )
import Data.Text (Text)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser(..))
import Gargantext.Database.Prelude (Cmd, HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.User (getUsersWithId, getUserHyperdata)
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import GHC.Generics (Generic)
import qualified Prelude as Prelude

data User = User
  { u_email     :: Text
  , u_hyperdata :: Maybe HyperdataUser
  , u_id        :: Int
  , u_username  :: Text }
  deriving (Show, Generic, GQLType)

-- | Arguments to the "user" query.
data UserArgs
  = UserArgs
    { user_id :: Int
    } deriving (Generic, GQLType)


-- | Function to resolve user from a query.
resolveUsers
  :: (HasConnectionPool env, HasConfig env)
  => UserArgs -> ResolverQ e (GargM env GargError) [User]
resolveUsers UserArgs { user_id } = do
  liftEither $ dbUsers user_id
--  user <- lift $ dbUser user_id
--  case user of
--    --Left err -> failure $ msg err
--    Left err -> error "fail"
--    Right u -> pure u

-- | Inner function to fetch the user from DB.
dbUsers :: Int -> Cmd err (Either Prelude.String [User])
dbUsers user_id = do
  users <- getUsersWithId user_id
--  users' <- if includeHyperdata
--    then mapM injectHyperdata (toUser <$> users)
--    else (pure $ toUser <$> users)
  users' <- mapM injectHyperdata $ toUser <$> users
  pure $ Right users'

toUser :: UserLight -> User
toUser (UserLight { .. }) = User { u_email = userLight_email
                                 , u_hyperdata = Nothing
                                 , u_id = userLight_id
                                 , u_username = userLight_username }

injectHyperdata :: User -> Cmd err User
injectHyperdata user@(User { .. }) = do
  hyperdata <- getUserHyperdata u_id
  case hyperdata of
    [] -> pure $ user
    (h:_) -> pure $ User { u_hyperdata = Just h, .. }
