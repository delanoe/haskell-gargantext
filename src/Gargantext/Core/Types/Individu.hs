{-|
Module      : Gargantext.Core.Types.Individu
Description : Short description
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Individu defintions
-}



module Gargantext.Core.Types.Individu
  where

import Data.Aeson
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Data.Swagger
import Data.Text (Text, pack, reverse)
import Gargantext.Database.Admin.Types.Node (NodeId, UserId)
import Gargantext.Prelude hiding (reverse)
import qualified Gargantext.Prelude.Crypto.Auth as Auth

-- FIXME UserName used twice
data User = UserDBId UserId | UserName Text | RootId NodeId | UserPublic
  deriving (Eq)

type Username = Text

type HashPassword    = Auth.PasswordHash Auth.Argon2
newtype GargPassword = GargPassword Text
  deriving (Generic)

instance Show GargPassword where
  show (GargPassword _) = "*GargPassword*"

instance ToJSON GargPassword
instance FromJSON GargPassword

instance ToSchema GargPassword
type Email          = Text
type UsernameMaster = Username
type UsernameSimple = Username

data NewUser a = NewUser { _nu_username :: Username
                         , _nu_email    :: Email
                         , _nu_password :: a
                         }
  deriving (Show)

arbitraryUsername :: [Username]
arbitraryUsername = {- ["gargantua"] <> -} users
  where
    users = zipWith (\a b -> a <> (pack . show) b) 
                    (repeat "user") ([1..20]::[Int])

arbitraryPassword :: [GargPassword]
arbitraryPassword = map (\u -> GargPassword (reverse u)) arbitraryUsername

-----------------------------------------------------------
toUserHash :: MonadIO m
         =>    NewUser GargPassword
         -> m (NewUser HashPassword)
toUserHash (NewUser u m (GargPassword p)) = do
  h <- Auth.createPasswordHash p
  pure $ NewUser u m h

-- TODO remove
arbitraryUsersHash :: MonadIO m
                  => m [NewUser HashPassword]
arbitraryUsersHash = mapM toUserHash arbitraryNewUsers

arbitraryNewUsers :: [NewUser GargPassword]
arbitraryNewUsers = map (\u -> NewUser u (u <> "@gargantext.org") (GargPassword $ reverse u))
                     arbitraryUsername


