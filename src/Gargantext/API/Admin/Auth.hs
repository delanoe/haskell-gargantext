{-|
Module      : Gargantext.API.Admin.Auth
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

{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.API.Admin.Auth
  ( auth
  , withAccess
  )
  where

import Control.Lens (view)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Servant
import Servant.Auth.Server
import qualified Gargantext.Prelude.Crypto.Auth as Auth

import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Admin.Types
import Gargantext.API.Prelude (HasJoseError(..), joseError, HasServerError, GargServerC)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.Types.Individu (User(..), Username, GargPassword(..))
import Gargantext.Database.Admin.Types.Node (NodeId(..), UserId)
import Gargantext.Database.Prelude (Cmd', CmdM, HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.User
import Gargantext.Database.Query.Tree (isDescendantOf, isIn)
import Gargantext.Database.Query.Tree.Root (getRoot)
import Gargantext.Database.Schema.Node (NodePoly(_node_id))
import Gargantext.Prelude hiding (reverse)

---------------------------------------------------

-- | Main functions of authorization

makeTokenForUser :: (HasSettings env, HasJoseError err)
                 => NodeId -> Cmd' env err Token
makeTokenForUser uid = do
  jwtS <- view $ settings . jwtSettings
  e <- liftBase $ makeJWT (AuthenticatedUser uid) jwtS Nothing
  -- TODO-SECURITY here we can implement token expiration ^^.
  either joseError (pure . toStrict . decodeUtf8) e
  -- TODO not sure about the encoding...

checkAuthRequest :: (HasSettings env, HasConnectionPool env, HasJoseError err, HasConfig env, HasMail env)
                 => Username
                 -> GargPassword
                 -> Cmd' env err CheckAuth
checkAuthRequest u (GargPassword p) = do
  candidate <- head <$> getUsersWith u
  case candidate of
    Nothing -> pure InvalidUser
    Just (UserLight id _u _email h) ->
      case Auth.checkPassword (Auth.mkPassword p) (Auth.PasswordHash h) of
        Auth.PasswordCheckFail    -> pure InvalidPassword
        Auth.PasswordCheckSuccess -> do
          muId <- head <$> getRoot (UserName u)
          case _node_id <$> muId of
            Nothing  -> pure InvalidUser
            Just uid -> do
              token <- makeTokenForUser uid
              pure $ Valid token uid id

auth :: (HasSettings env, HasConnectionPool env, HasJoseError err, HasConfig env, HasMail env)
     => AuthRequest -> Cmd' env err AuthResponse
auth (AuthRequest u p) = do
  checkAuthRequest' <- checkAuthRequest u p
  case checkAuthRequest' of
    InvalidUser     -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid user")
    InvalidPassword -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid password")
    Valid to trId uId   -> pure $ AuthResponse (Just $ AuthValid to trId uId) Nothing

--type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

{-
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: forall env. env
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck _env (BasicAuthData login password) = pure $
  maybe Indefinite Authenticated $ TODO
-}

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
              Proxy api -> Proxy m -> UserId -> PathId ->
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
