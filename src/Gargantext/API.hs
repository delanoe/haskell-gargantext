{-|
Module      : Gargantext.API
Description : REST API declaration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main (RESTful) API of the instance Gargantext.

The Garg-API is typed to derive the documentation, the mock and tests.

This API is indeed typed in order to be able to derive both the server
and the client sides.

The Garg-API-Monad enables:
  - Security (WIP)
  - Features (WIP)
  - Database connection (long term)
  - In Memory stack management (short term)
  - Logs (WIP)

Thanks to Yann Esposito for our discussions at the start and to Nicolas
Pouillard (who mainly made it).

-}

{-# LANGUAGE ScopedTypeVariables  #-}

module Gargantext.API
      where

import Control.Exception (finally)
import Control.Lens
import Control.Monad.Reader (runReaderT)
import Data.List (lookup)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStrLn)
import Data.Validity
import GHC.Base (Applicative)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Auth.Types (AuthContext)
import Gargantext.API.Admin.Settings (newEnv)
import Gargantext.API.Admin.Types (FireWall(..), PortNumber, cookieSettings, jwtSettings, settings)
import Gargantext.API.Ngrams (saveRepo)
import Gargantext.API.Prelude
import Gargantext.API.Routes
import Gargantext.API.Server (server)
import Gargantext.Core.NodeStory
import Gargantext.Prelude hiding (putStrLn)
import Network.HTTP.Types hiding (Query)
import Network.Wai
import Network.Wai.Handler.Warp hiding (defaultSettings)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant
import System.IO (FilePath)


data Mode = Dev | Mock | Prod 
       deriving (Show, Read, Generic)

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: Mode -> PortNumber -> FilePath -> IO ()
startGargantext mode port file = do
  env <- newEnv port file
  portRouteInfo port
  app <- makeApp env
  mid <- makeDevMiddleware mode
  run port (mid app) `finally` stopGargantext env

portRouteInfo :: PortNumber -> IO ()
portRouteInfo port = do
  putStrLn "      ----Main Routes-----      "
  putStrLn $ "http://localhost:" <> toUrlPiece port <> "/index.html"
  putStrLn $ "http://localhost:" <> toUrlPiece port <> "/swagger-ui"

-- TODO clean this Monad condition (more generic) ?
stopGargantext :: HasNodeStorySaver env => env -> IO ()
stopGargantext env = do
  putStrLn "----- Stopping gargantext -----"
  runReaderT saveRepo env

{-
startGargantextMock :: PortNumber -> IO ()
startGargantextMock port = do
  portRouteInfo port
  application <- makeMockApp . MockEnv $ FireWall False
  run port application
-}

----------------------------------------------------------------------

fireWall :: Applicative f => Request -> FireWall -> f Bool
fireWall req fw = do
    let origin = lookup "Origin" (requestHeaders req)
    let host   = lookup "Host"   (requestHeaders req)

    if  origin == Just (encodeUtf8 "http://localhost:8008")
       && host == Just (encodeUtf8 "localhost:3000")
       || (not $ unFireWall fw)

       then pure True
       else pure False

{-
-- makeMockApp :: Env -> IO (Warp.Settings, Application)
makeMockApp :: MockEnv -> IO Application
makeMockApp env = do
    let serverApp = appMock

    -- logWare <- mkRequestLogger def { destination = RequestLogger.Logger $ env^.logger }
    --logWare <- mkRequestLogger def { destination = RequestLogger.Logger "/tmp/logs.txt" }
    let checkOriginAndHost app req resp = do
            blocking <- fireWall req (env ^. menv_firewall)
            case blocking  of
                True  -> app req resp
                False -> resp ( responseLBS status401 [] 
                              "Invalid Origin or Host header")
        
    let corsMiddleware = cors $ \_ -> Just CorsResourcePolicy
--          { corsOrigins        = Just ([env^.settings.allowedOrigin], False)
            { corsOrigins        = Nothing --  == /*
            , corsMethods        = [ methodGet   , methodPost   , methodPut
                                   , methodDelete, methodOptions, methodHead]
            , corsRequestHeaders = ["authorization", "content-type"]
            , corsExposedHeaders = Nothing
            , corsMaxAge         = Just ( 60*60*24 ) -- one day
            , corsVaryOrigin     = False
            , corsRequireOrigin  = False
            , corsIgnoreFailures = False
            }

    --let warpS = Warp.setPort (8008 :: Int)   -- (env^.settings.appPort)
    --          $ Warp.defaultSettings
    
    --pure (warpS, logWare $ checkOriginAndHost $ corsMiddleware $ serverApp)
    pure $ logStdoutDev $ checkOriginAndHost $ corsMiddleware $ serverApp
-}


makeDevMiddleware :: Mode -> IO Middleware
makeDevMiddleware mode = do
-- logWare <- mkRequestLogger def { destination = RequestLogger.Logger $ env^.logger }
-- logWare <- mkRequestLogger def { destination = RequestLogger.Logger "/tmp/logs.txt" }
--    let checkOriginAndHost app req resp = do
--            blocking <- fireWall req (env ^. menv_firewall)
--            case blocking  of
--                True  -> app req resp
--                False -> resp ( responseLBS status401 [] 
--                              "Invalid Origin or Host header")
--
    let corsMiddleware = cors $ \_ -> Just CorsResourcePolicy
--          { corsOrigins        = Just ([env^.settings.allowedOrigin], False)
            { corsOrigins        = Nothing --  == /*
            , corsMethods        = [ methodGet   , methodPost   , methodPut
                                   , methodDelete, methodOptions, methodHead]
            , corsRequestHeaders = ["authorization", "content-type"]
            , corsExposedHeaders = Nothing
            , corsMaxAge         = Just ( 60*60*24 ) -- one day
            , corsVaryOrigin     = False
            , corsRequireOrigin  = False
            , corsIgnoreFailures = False
            }

    --let warpS = Warp.setPort (8008 :: Int)   -- (env^.settings.appPort)
    --          $ Warp.defaultSettings

    --pure (warpS, logWare . checkOriginAndHost . corsMiddleware)
    case mode of
      Prod -> pure $ logStdout . corsMiddleware
      _    -> pure $ logStdoutDev . corsMiddleware

---------------------------------------------------------------------
-- | API Global
---------------------------------------------------------------------

---------------------------


-- TODO-SECURITY admin only: withAdmin
-- Question: How do we mark admins?
{-
serverGargAdminAPI :: GargServer GargAdminAPI
serverGargAdminAPI =  roots
                 :<|> nodesAPI
-}

---------------------------------------------------------------------
--gargMock :: Server GargAPI
--gargMock = mock apiGarg Proxy
---------------------------------------------------------------------
makeApp :: EnvC env => env -> IO Application
makeApp env = serveWithContext api cfg <$> server env
  where
    cfg :: Servant.Context AuthContext
    cfg = env ^. settings . jwtSettings
       :. env ^. settings . cookieSettings
    -- :. authCheck env
       :. EmptyContext

--appMock :: Application
--appMock = serve api (swaggerFront :<|> gargMock :<|> serverStatic)
---------------------------------------------------------------------
api :: Proxy API
api  = Proxy

apiGarg :: Proxy GargAPI
apiGarg  = Proxy
---------------------------------------------------------------------

{- UNUSED
--import GHC.Generics (D1, Meta (..), Rep, Generic)
--import GHC.TypeLits (AppendSymbol, Symbol)
---------------------------------------------------------------------
-- Type Family for the Documentation
type family TypeName (x :: *) :: Symbol where
    TypeName Int  = "Int"
    TypeName Text = "Text"
    TypeName x    = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: *) :: Symbol where
    GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))
-}
