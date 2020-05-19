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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Core.Types.Individu
  where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack, reverse)
import Gargantext.Database.Admin.Types.Node (NodeId, UserId)
import Gargantext.Prelude hiding (reverse)
import qualified Gargantext.Core.Auth as Auth

-- FIXME UserName used twice
data User = UserDBId UserId | UserName Text | RootId NodeId
  deriving (Eq)

type Username = Text
type Password = Text
type Email    = Text

type UsernameMaster = Username
type UsernameSimple = Username


arbitraryUsername :: [Username]
arbitraryUsername = ["gargantua"] <> users
  where
    users = zipWith (\a b -> a <> (pack . show) b) 
                    (repeat "user") ([1..20]::[Int])

arbitraryPassword :: [Password]
arbitraryPassword = map reverse arbitraryUsername

-----------------------------------------------------------

arbitraryUsersHash :: MonadIO m
                  => m [(Username, Email, Auth.PasswordHash Auth.Argon2)]
arbitraryUsersHash = mapM userHash arbitraryUsers

userHash :: MonadIO m
                  => (Username, Email, Password)
                  -> m (Username, Email, Auth.PasswordHash Auth.Argon2)
userHash (u,m,p) = do
  h <- Auth.createPasswordHash p
  pure (u, m, h)

arbitraryUsers :: [(Username, Email, Password)]
arbitraryUsers = map (\u -> (u, u <> "@gargantext.org", reverse u)) arbitraryUsername

