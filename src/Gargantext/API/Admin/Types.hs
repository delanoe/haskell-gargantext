-- |

{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Types where

import Control.Lens
import Control.Monad.Logger
import Data.ByteString (ByteString)
import GHC.Enum
import GHC.Generics (Generic)
import Gargantext.Prelude
import Servant.Auth.Server (JWTSettings, CookieSettings(..))
import Servant.Client (BaseUrl)


type PortNumber = Int

data SendEmailType = SendEmailViaAws
                   | LogEmailToConsole
                   | WriteEmailToFile
    deriving (Show, Read, Enum, Bounded, Generic)

data Settings = Settings
    { _allowedOrigin   :: !ByteString   -- allowed origin for CORS
    , _allowedHost     :: !ByteString   -- allowed host for CORS
    , _appPort         :: !PortNumber
    , _logLevelLimit   :: !LogLevel -- log level from the monad-logger package
--    , _dbServer        :: Text
--    ^ this is not used yet
    , _jwtSettings     :: !JWTSettings
    , _cookieSettings  :: !CookieSettings
    , _sendLoginEmails :: !SendEmailType
    , _scrapydUrl      :: !BaseUrl
    }

makeLenses ''Settings

class HasSettings env where
  settings :: Getter env Settings

instance HasSettings Settings where
  settings = identity

data FireWall = FireWall { unFireWall :: Bool }
