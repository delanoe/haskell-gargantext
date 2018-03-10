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

Credits: http://blog.wuzzeb.org/full-stack-web-haskell/libraries.html
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeOperators               #-}

module Gargantext.API.Auth
      where

------------------------------------------------------------------------

import GHC.Int (Int64)
import GHC.Generics (Generic)

import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Monad.Reader

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Either (Either(Left, Right))
import Data.Time.Clock.POSIX (POSIXTime(), getPOSIXTime)
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Servant
import qualified Jose.Jwa as Jose
import qualified Jose.Jwt as Jose

import Gargantext.Prelude hiding (drop)
import Gargantext.API
import Gargantext.API.Settings
------------------------------------------------------------------------

type UserId = Int

-- | User credentials extracted from the JWT token
--data Auth = Auth { username :: Text
--                 , password :: Text
--                 } deriving (Generics)
data UserCredentials = UserCredentials { _credId            :: UserId
                                       , _credEmail         :: Text
                                       , _credEmailVerified :: Bool
                                       }
    deriving (Show, Eq, Generic)

makeLenses ''UserCredentials

-- | There are two kinds of tokens.  When reseting the password, we send
-- a token via email.  When a user logs in, we send a token to the browser.
data TokenAudience = TokenSentViaEmail | TokenSentToBrowser | TokenForLiveUser
    deriving (Show, Eq, Generic)

-- | The contents of the Jwt token is an encoding of this structure.
data Token = Token { _tokenCreds  :: UserCredentials
                   , _issuedP     :: POSIXTime
                   , _expiredP    :: POSIXTime
                   , _jwtAudience :: TokenAudience
                   }
    deriving (Show, Eq, Generic)

makeLenses ''Token

instance Aeson.FromJSON Token where
    parseJSON = Aeson.withObject "web token" $ \o -> do
        
        aud :: Text <- o .: "aud"
        aud' <- case aud of
                    "mycompany:email" -> pure TokenSentViaEmail
                    "mycompany:web"   -> pure TokenSentToBrowser
                    _                 -> panic "Invalid audience for token"
        
        Token <$> (UserCredentials <$> o .: "sub" <*> o .: "email" <*> o .: "email_verified")
              <*> (fromInteger <$> o .: "iat")
              <*> (fromInteger <$> o .: "exp")
              <*> pure aud'

instance Aeson.ToJSON Token where
    toJSON (Token ucreds i e a) = Aeson.object
        [ "sub"            .= (ucreds^.credId)
        , "email"          .= (ucreds^.credEmail)
        , "email_verified" .= (ucreds^.credEmailVerified)
        , "iat"            .= (round i :: Int64)
        , "exp"            .= (round e :: Int64)
        , "aud"            .= case a of
                                TokenSentViaEmail  -> pack "mycompany:email"
                                TokenSentToBrowser -> pack "mycompany:web"
                                TokenForLiveUser   -> pack "mycompany:live"
        ]


newtype UnverifiedJwtToken = UnverifiedJwtToken Text
  deriving (Show)
  -- deriving (Show, FromHttpApiData)

type JwtAuthHeader = Header "authorization" UnverifiedJwtToken

type MyAPIWithAuth = JwtAuthHeader :> GargAPI


-- Datastorage

--isMemberOfTeam :: UserCredentials -> Team -> Bool
--user `isMemberOfTeam` team = {- implementation here -}
--
--loadTeam :: UserCredentials -> TeamKey -> SqlPersistM Team
--loadTeam ucreds teamkey = do
--    mteam <- get teamkey
--    case mteam of
--        Nothing -> throwM DocumentNotFound
--        Just t | ucreds `isMemberOfTeam` t -> return t
--               | otherwise -> throwM Unauthorized



createJwt :: TokenAudience -> UserCredentials -> Env -> Servant.Handler UnverifiedJwtToken
createJwt aud ucreds env = do
    now <- liftIO getPOSIXTime
    expire <- pure $ case aud of
            TokenSentViaEmail -> 15*60 -- 15 minutes
            TokenForLiveUser  -> 60*60 -- 1 hour
            _                 -> 60*60
    let key = env^.settings.jwtSecret

    let token = Token { _tokenCreds = ucreds
                      , _issuedP = now
                      , _expiredP = now + expire
                      , _jwtAudience = aud
                      }
    mjwt <- liftIO $ Jose.encode [key] (Jose.JwsEncoding Jose.HS256) (Jose.Claims $ toStrict $ Aeson.encode token)
    case mjwt of
        Left _ -> throwError err500 {errBody = "Unable to authenticate"}
        Right jwt -> pure $ UnverifiedJwtToken $ decodeUtf8 $ Jose.unJwt jwt



verifyJwt :: UnverifiedJwtToken -> Env -> Servant.Handler Token
verifyJwt (UnverifiedJwtToken unverifiedText) env = do
    key <- pure $ env^.settings.jwtSecret
    mjwtContent <- liftIO $ Jose.decode [key] (Just $ Jose.JwsEncoding Jose.HS256) $ encodeUtf8 unverifiedText
    jwt <- case mjwtContent of
            Right (Jose.Jws (_, jwt)) -> pure $ jwt
            _ -> throwError err401 { errBody = "Invalid javascript web token" }

    case Aeson.eitherDecode (fromStrict jwt) of
        Left _ -> throwError err401 { errBody = "Unable to parse jwt claims" }

        Right token -> do
            now <- liftIO getPOSIXTime
            when (token^.expiredP <= now) $
                throwError err401 { errBody = "Expired jwt token" }
            pure token


-- | Verify and decode a token
verifyWebJwt :: Maybe UnverifiedJwtToken -> Env -> Servant.Handler (Maybe Token)
verifyWebJwt Nothing _ = return Nothing
verifyWebJwt (Just (UnverifiedJwtToken x)) env = do
    let unverifiedToken = if "Bearer " `isPrefixOf` x then drop 7 x else x
    token <- verifyJwt (UnverifiedJwtToken unverifiedToken) env
    case token^.jwtAudience of
        TokenSentViaEmail -> throwError err403 { errBody = "Cannot use email token for authentication" }
        TokenForLiveUser -> pure (Just token)
        _                -> pure Nothing




