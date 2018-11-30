{-|
Module      : Gargantext.API.Auth
Description : Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main authorisation of Gargantext are managed in this module

-- 1: Implement the Server / Client JWT authentication
      -> Client towards Python Backend
      -> Server towards Purescript Front-End

-- 2: Implement the Auth API backend
    https://github.com/haskell-servant/servant-auth

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.API.Auth
      where

import Data.Aeson.TH (deriveJSON)
import Data.List (elem)
import Data.Swagger
import Data.Text (Text, reverse)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Types.Node (NodePoly(_node_id))
import Gargantext.Prelude hiding (reverse)
import Test.QuickCheck (elements, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

---------------------------------------------------

-- | Main types for AUTH API
type Username = Text
type Password = Text

data AuthRequest = AuthRequest { _authReq_username :: Username
                               , _authReq_password :: Password
                               }
  deriving (Generic)

-- TODO: Use an HTTP error to wrap AuthInvalid
data AuthResponse = AuthResponse { _authRes_valid :: Maybe AuthValid
                                 , _authRes_inval :: Maybe AuthInvalid
                                 }
  deriving (Generic)

data AuthInvalid = AuthInvalid { _authInv_message :: Text }
  deriving (Generic)

data AuthValid = AuthValid { _authVal_token   :: Token
                           , _authVal_tree_id :: TreeId
                           }
  deriving (Generic)

type Token  = Text
type TreeId = Int

-- | Main functions of authorization


-- | Main types of authorization
data CheckAuth = InvalidUser | InvalidPassword | Valid Token TreeId
  deriving (Eq)

arbitraryUsername :: [Username]
arbitraryUsername = ["gargantua", "user1", "user2"]

arbitraryPassword :: [Password]
arbitraryPassword = map reverse arbitraryUsername

checkAuthRequest :: Username -> Password -> Connection -> IO CheckAuth
checkAuthRequest u p c
  | not (u `elem` arbitraryUsername) = pure InvalidUser
  | u /= reverse p = pure InvalidPassword
  | otherwise = do
      muId <- getRoot u c
      pure $ maybe InvalidUser (Valid "token" . _node_id) $ head muId

auth' :: Connection -> AuthRequest -> IO AuthResponse
auth' c (AuthRequest u p) = do
  checkAuthRequest' <- checkAuthRequest u p c
  case checkAuthRequest' of
    InvalidUser     -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid user")
    InvalidPassword -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid password")
    Valid to trId   -> pure $ AuthResponse (Just $ AuthValid to trId) Nothing

-- | Instances
$(deriveJSON (unPrefix "_authReq_") ''AuthRequest)
instance ToSchema AuthRequest

instance Arbitrary AuthRequest where
  arbitrary = elements [ AuthRequest u p
                       | u <- arbitraryUsername
                       , p <- arbitraryPassword
                       ]

$(deriveJSON (unPrefix "_authRes_") ''AuthResponse)
instance ToSchema AuthResponse
instance Arbitrary AuthResponse where
  arbitrary = oneof [ AuthResponse Nothing . Just      <$> arbitrary
                    , flip AuthResponse Nothing . Just <$> arbitrary ]

$(deriveJSON (unPrefix "_authInv_") ''AuthInvalid)
instance ToSchema AuthInvalid
instance Arbitrary AuthInvalid where
  arbitrary = elements [ AuthInvalid m 
                       | m <- [ "Invalid user", "Invalid password"]
                       ]

$(deriveJSON (unPrefix "_authVal_") ''AuthValid)
instance ToSchema AuthValid
instance Arbitrary AuthValid where
  arbitrary = elements [ AuthValid to tr
                       | to <- ["token0", "token1"]
                       , tr <- [1..3]
                       ]

