{-# LANGUAGE DeriveAnyClass #-}

module Gargantext.API.GraphQL.User where

import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Morpheus.Types
  ( GQLType
  , ResolverQ
  , liftEither
  )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Database.Prelude (Cmd, HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.User (getUsersWithId)
import Gargantext.Database.Schema.User (UserLight)
import Gargantext.Prelude
import GHC.Generics (Generic)
import qualified Prelude as Prelude


-- | Arguments to the "user" query.
data UserArgs
  = UserArgs
    { user_id :: Int
    , includeHyperdata :: Maybe Bool
    } deriving (Generic, GQLType)


-- | Function to resolve user from a query.
resolveUsers
  :: (HasConnectionPool env, HasConfig env)
  => UserArgs -> ResolverQ e (GargM env GargError) [UserLight]
resolveUsers UserArgs { user_id, includeHyperdata } = do
  let _hyp = fromMaybe False includeHyperdata
  liftEither $ dbUsers user_id
--  user <- lift $ dbUser user_id
--  case user of
--    --Left err -> failure $ msg err
--    Left err -> error "fail"
--    Right u -> pure u

-- | Inner function to fetch the user from DB.
dbUsers :: Int -> Cmd err (Either Prelude.String [UserLight])
dbUsers user_id = do
  users <- getUsersWithId user_id
  pure $ Right users
