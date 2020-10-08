{-|
Module      : Gargantext.API.Server
Description : REST API declaration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables  #-}

---------------------------------------------------------------------
module Gargantext.API.Server where
---------------------------------------------------------------------
import Control.Monad.Except (withExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Data.Version (showVersion)
import Servant
import Servant.Swagger.UI (swaggerSchemaUIServer)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Paths_gargantext           as PG -- cabal magic build module

import qualified Gargantext.API.Public      as Public

import Gargantext.API.Admin.Auth (AuthContext, auth)
import Gargantext.API.Admin.FrontEnd (frontEndServer)
import Gargantext.API.Prelude
import Gargantext.API.Routes
import Gargantext.API.Swagger (swaggerDoc)
import Gargantext.API.ThrowAll (serverPrivateGargAPI)
import Gargantext.Prelude


serverGargAPI :: Text -> GargServerM env err GargAPI
serverGargAPI baseUrl -- orchestrator
       =  auth
     :<|> gargVersion
     :<|> serverPrivateGargAPI
     :<|> Public.api baseUrl

  --   :<|> orchestrator
  where
    gargVersion :: GargServer GargVersion
    gargVersion = pure (cs $ showVersion PG.version)

-- | Server declarations
server :: forall env. EnvC env => env -> Text -> IO (Server API)
server env baseUrl = do
  -- orchestrator <- scrapyOrchestrator env
  pure $  swaggerSchemaUIServer swaggerDoc
     :<|> hoistServerWithContext
            (Proxy :: Proxy GargAPI)
            (Proxy :: Proxy AuthContext)
            transform
            (serverGargAPI baseUrl)
     :<|> frontEndServer
  where
    transform :: forall a. GargM env GargError a -> Handler a
    transform = Handler . withExceptT showAsServantErr . (`runReaderT` env)


showAsServantErr :: GargError -> ServerError
showAsServantErr (GargServerError err) = err
showAsServantErr a = err500 { errBody = BL8.pack $ show a }