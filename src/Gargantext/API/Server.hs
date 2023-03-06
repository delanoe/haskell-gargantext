{-|
Module      : Gargantext.API.Server
Description : REST API declaration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}

---------------------------------------------------------------------
module Gargantext.API.Server where
---------------------------------------------------------------------
import Control.Lens ((^.))
import Control.Monad.Except (withExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Data.Version (showVersion)
import Servant
import Servant.Swagger.UI (swaggerSchemaUIServer)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Paths_gargantext           as PG -- cabal magic build module

import qualified Gargantext.API.Public      as Public

import Gargantext.API.Admin.Auth.Types (AuthContext)
import Gargantext.API.Admin.Auth (auth, forgotPassword, forgotPasswordAsync)
import Gargantext.API.Admin.EnvTypes (Env)
import Gargantext.API.Admin.FrontEnd (frontEndServer)
import qualified Gargantext.API.GraphQL as GraphQL
import Gargantext.API.Prelude
import Gargantext.API.Routes
import Gargantext.API.Swagger (swaggerDoc)
import Gargantext.API.ThrowAll (serverPrivateGargAPI)
import Gargantext.Database.Query.Table.Node.Error (NodeError(..))
import Gargantext.Database.Prelude (hasConfig)
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_url_backend_api)


serverGargAPI :: Text -> ServerT GargAPI (GargM Env GargError)
serverGargAPI baseUrl -- orchestrator
       =  auth
     :<|> forgotPassword
     :<|> forgotPasswordAsync
     :<|> gargVersion
     :<|> serverPrivateGargAPI
     :<|> Public.api baseUrl

  --   :<|> orchestrator
  where
    gargVersion :: GargServer GargVersion
    gargVersion = pure (cs $ showVersion PG.version)

-- | Server declarations
server :: Env -> IO (Server API)
server env = do
  -- orchestrator <- scrapyOrchestrator env
  pure $  swaggerSchemaUIServer swaggerDoc
     :<|> hoistServerWithContext
            (Proxy :: Proxy GargAPI)
            (Proxy :: Proxy AuthContext)
            transform
            (serverGargAPI (env ^. hasConfig . gc_url_backend_api))
     :<|> hoistServerWithContext
            (Proxy :: Proxy GraphQL.API)
            (Proxy :: Proxy AuthContext)
            transform
            GraphQL.api
     :<|> frontEndServer
  where
    transform :: forall a. GargM Env GargError a -> Handler a
    transform = Handler . withExceptT showAsServantErr . (`runReaderT` env)


showAsServantErr :: GargError -> ServerError
showAsServantErr (GargNodeError err@NoListFound) = err404 { errBody = BL8.pack $ show err }
showAsServantErr (GargNodeError err@NoRootFound) = err404 { errBody = BL8.pack $ show err }
showAsServantErr (GargNodeError err@NoCorpusFound) = err404 { errBody = BL8.pack $ show err }
showAsServantErr (GargNodeError err@NoUserFound) = err404 { errBody = BL8.pack $ show err }
showAsServantErr (GargNodeError err@(DoesNotExist _)) = err404 { errBody = BL8.pack $ show err }
showAsServantErr (GargServerError err) = err
showAsServantErr a = err500 { errBody = BL8.pack $ show a }
