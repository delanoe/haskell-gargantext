{-|
Module      : Gargantext.API.Auth
Description : Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main authorization of Gargantext are managed in this module

-- 1: Implement the Server / Client JWT authentication
      -> Client towards Python Backend
      -> Server towards Purescript Front-End

-- 2: Implement the Auth API backend
    https://github.com/haskell-servant/servant-auth

TODO-ACCESS Critical

-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Auth
      where

import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (deriveJSON)
import Data.List (elem)
import Data.Swagger
import Data.Text (Text, reverse)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.API.Settings
import Gargantext.API.Types (HasJoseError(..), joseError, HasServerError, GargServerC)
--import Gargantext.API.Types (HasJoseError(..), joseError, HasServerError, serverError, GargServerC)
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Tree (isDescendantOf, isIn)
import Gargantext.Database.Types.Node (NodePoly(_node_id), NodeId(..), UserId, ListId, DocId)
import Gargantext.Database.Utils (Cmd', CmdM, HasConnection)
import Gargantext.Prelude hiding (reverse)
import Test.QuickCheck (elements, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Gargantext.Core.Types.Individu (Username, Password, arbitraryUsername, arbitraryPassword)

---------------------------------------------------

-- | Main types for AUTH API
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
type TreeId = NodeId

-- | Main functions of authorization

-- | Main types of authorization
data CheckAuth = InvalidUser | InvalidPassword | Valid Token TreeId
  deriving (Eq)

makeTokenForUser :: (HasSettings env, HasJoseError err)
                 => NodeId -> Cmd' env err Token
makeTokenForUser uid = do
  jwtS <- view $ settings . jwtSettings
  e <- liftIO $ makeJWT (AuthenticatedUser uid) jwtS Nothing
  -- TODO-SECURITY here we can implement token expiration ^^.
  either joseError (pure . toStrict . decodeUtf8) e
  -- TODO not sure about the encoding...

checkAuthRequest :: (HasSettings env, HasConnection env, HasJoseError err)
                 => Username -> Password -> Cmd' env err CheckAuth
checkAuthRequest u p
  | not (u `elem` arbitraryUsername) = pure InvalidUser
  | u /= reverse p = pure InvalidPassword
  | otherwise = do
      muId <- head <$> getRoot u
      case _node_id <$> muId of
        Nothing  -> pure InvalidUser
        Just uid -> do
          token <- makeTokenForUser uid
          pure $ Valid token uid

auth :: (HasSettings env, HasConnection env, HasJoseError err)
     => AuthRequest -> Cmd' env err AuthResponse
auth (AuthRequest u p) = do
  checkAuthRequest' <- checkAuthRequest u p
  case checkAuthRequest' of
    InvalidUser     -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid user")
    InvalidPassword -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid password")
    Valid to trId   -> pure $ AuthResponse (Just $ AuthValid to trId) Nothing

newtype AuthenticatedUser = AuthenticatedUser
  { _authUser_id :: NodeId
  } deriving (Generic)

$(deriveJSON (unPrefix "_authUser_") ''AuthenticatedUser)
instance ToSchema AuthenticatedUser where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authUser_")
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

--type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

-- TODO-SECURITY why is the CookieSettings necessary?
type AuthContext = '[JWTSettings, CookieSettings] -- , BasicAuthCfg

{-
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: forall env. env
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck _env (BasicAuthData login password) = pure $
  maybe Indefinite Authenticated $ TODO
-}

-- | Instances
$(deriveJSON (unPrefix "_authReq_") ''AuthRequest)
instance ToSchema AuthRequest where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authReq_")

instance Arbitrary AuthRequest where
  arbitrary = elements [ AuthRequest u p
                       | u <- arbitraryUsername
                       , p <- arbitraryPassword
                       ]

$(deriveJSON (unPrefix "_authRes_") ''AuthResponse)
instance ToSchema AuthResponse where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authRes_")
instance Arbitrary AuthResponse where
  arbitrary = oneof [ AuthResponse Nothing . Just      <$> arbitrary
                    , flip AuthResponse Nothing . Just <$> arbitrary ]

$(deriveJSON (unPrefix "_authInv_") ''AuthInvalid)
instance ToSchema AuthInvalid where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authInv_")
instance Arbitrary AuthInvalid where
  arbitrary = elements [ AuthInvalid m 
                       | m <- [ "Invalid user", "Invalid password"]
                       ]

$(deriveJSON (unPrefix "_authVal_") ''AuthValid)
instance ToSchema AuthValid where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authVal_")
instance Arbitrary AuthValid where
  arbitrary = elements [ AuthValid to tr
                       | to <- ["token0", "token1"]
                       , tr <- [1..3]
                       ]

data PathId = PathNode NodeId | PathNodeNode ListId DocId

withAccessM :: (CmdM env err m, HasServerError err)
            => UserId
            -> PathId
            -> m a
            -> m a
withAccessM uId (PathNode id) m = do
  d <- id `isDescendantOf` NodeId uId
  if d then m else m -- serverError err401

withAccessM uId (PathNodeNode cId docId) m = do
  _a <- isIn cId docId -- TODO use one query for all ?
  _d <- cId `isDescendantOf` NodeId uId
  if True -- a && d
     then m
     else m

withAccess :: forall env err m api.
              (GargServerC env err m, HasServer api '[]) =>
              Proxy api -> Proxy m ->
              UserId -> PathId ->
              ServerT api m -> ServerT api m
withAccess p _ uId id = hoistServer p f
  where
    f :: forall a. m a -> m a
    f = withAccessM uId id

{- | Collaborative Schema
User at his root can create Teams Folder
User can create Team in Teams Folder.
User can invite User in Team as NodeNode only if Team in his parents.
All users can access to the Team folder as if they were owner.
-}
