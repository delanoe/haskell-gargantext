{-|
Module      : Gargantext.API.Settings
Description : Settings of the API (Server and Client)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleInstances           #-}

module Gargantext.API.Settings
    where

import System.Log.FastLogger
import GHC.Enum
import GHC.Generics (Generic)
import Prelude (Bounded())
import System.Environment (lookupEnv)

-- import Control.Applicative ((<*>))

import Data.Maybe (fromMaybe)
import Data.Either (either)
-- import Data.Map
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Internal

import Servant
import Web.HttpApiData (parseUrlPiece)
import qualified Jose.Jwk as Jose
import qualified Jose.Jwa as Jose

import Control.Monad.Logger
import Control.Lens
import Gargantext.Prelude


data SendEmailType = SendEmailViaAws
                   | LogEmailToConsole
                   | WriteEmailToFile
    deriving (Show, Read, Enum, Bounded, Generic)


data Settings = Settings
    { _allowedOrigin   :: ByteString -- ^ allowed origin for CORS
    , _allowedHost     :: ByteString   -- ^ allowed host for CORS
    , _appPort         :: Int
    , _logLevelLimit   :: LogLevel -- ^ log level from the monad-logger package
    , _dbServer        :: Text
    , _jwtSecret       :: Jose.Jwk -- ^ key from the jose-jwt package
    , _sendLoginEmails :: SendEmailType
    }

makeLenses ''Settings


parseJwk :: Text -> Jose.Jwk
parseJwk secretStr = jwk
    where
        secretBs = encodeUtf8 secretStr
        jwk      = Jose.SymmetricJwk secretBs 
                                     Nothing 
                                     Nothing 
                                     (Just $ Jose.Signed Jose.HS256)

devSettings :: Settings
devSettings = Settings
    { _allowedOrigin = "http://localhost:8008"
    , _allowedHost = "localhost:3000"
    , _appPort = 3000
    , _logLevelLimit = LevelDebug
    , _dbServer = "localhost"
    -- generate with dd if=/dev/urandom bs=1 count=32 | base64
    -- make sure jwtSecret differs between development and production, because you do not want
    -- your production key inside source control.
    , _jwtSecret = parseJwk "MVg0YAPVSPiYQc/qIs/rV/X32EFR0zOJWfHFgMbszMw="
    , _sendLoginEmails = LogEmailToConsole
    }



reqSetting :: FromHttpApiData a => Text -> IO a
reqSetting name = do
    e <- fromMaybe (panic $ "Missing " <> name) <$> lookupEnv (unpack name)
    pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

optSetting :: FromHttpApiData a => Text -> a -> IO a
optSetting name d = do
    me <- lookupEnv (unpack name)
    case me of
        Nothing -> pure d
        Just e -> pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

--settingsFromEnvironment :: IO Settings
--settingsFromEnvironment =
--    Settings <$> (encodeUtf8 <$> reqSetting "ALLOWED_ORIGIN")
--             <*> (encodeUtf8 <$> reqSetting "ALLOWED_HOST")
--             <*> optSetting "PORT" 3000
--             <*> (parseLogLevel <$> optSetting "LOG_LEVEL" "warn")
--             <*> reqSetting "DB_SERVER"
--             <*> (parseJwk <$> reqSetting "JWT_SECRET")
--             <*> optSetting "SEND_EMAIL" SendEmailViaAws



data Env = Env
    { _settings :: Settings
    , _logger   :: LoggerSet
    -- , _dbConfig :: ConnectionPool -- from Database.Persist.Postgresql
    }

makeLenses ''Env
createEnv :: Settings -> IO Env
createEnv _ = undefined {- implementation here: connect to db, init logger, etc -}
