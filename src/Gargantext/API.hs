{-|
Module      : Gargantext.API
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main REST API of Gargantext (both Server and Client sides)

TODO App type, the main monad in which the bot code is written with.
Provide config, state, logs and IO
 type App m a =  ( MonadState AppState m
                 , MonadReader Conf m
                 , MonadLog (WithSeverity Doc) m
                 , MonadIO m) => m a
Thanks @yannEsposito for this.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE UndecidableInstances        #-}

---------------------------------------------------------------------
module Gargantext.API
      where
---------------------------------------------------------------------
import           Gargantext.Prelude

import           System.IO (FilePath, print)

import           GHC.Generics (D1, Meta (..), Rep)
import           GHC.TypeLits (AppendSymbol, Symbol)

import           Control.Lens
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Data.Text (Text, pack)
--import qualified Data.Set as Set

import           Database.PostgreSQL.Simple (Connection, connect)

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant
import           Servant.Mock (mock)
import           Servant.Swagger
import           Servant.Swagger.UI
-- import Servant.API.Stream

--import Gargantext.API.Swagger
import Gargantext.API.FrontEnd (FrontEndAPI, frontEndServer)

import Gargantext.API.Node ( Roots    , roots
                           , NodeAPI  , nodeAPI
                           , NodesAPI , nodesAPI
                           )
import Gargantext.API.Count ( CountAPI, count, Query)
import Gargantext.Database.Utils (databaseParameters)

---------------------------------------------------------------------

import GHC.Base (Applicative)
-- import Control.Lens

import Data.List (lookup)
import Data.Text.Encoding (encodeUtf8)

--import Network.Wai (Request, requestHeaders, responseLBS)
import Network.Wai (Request, requestHeaders)
--import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

-- import Network.Wai.Middleware.RequestLogger
-- import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import Network.HTTP.Types hiding (Query)


-- import Gargantext.API.Settings

data FireWall = FireWall { unFireWall :: Bool }

fireWall :: Applicative f => Request -> FireWall -> f Bool
fireWall req fw = do
    let origin = lookup "Origin" (requestHeaders req)
    let host   = lookup "Host"   (requestHeaders req)

    let hostOk   = Just (encodeUtf8 "localhost:3000")
    let originOk = Just (encodeUtf8 "http://localhost:8008")

    if origin == originOk && host == hostOk || (not $ unFireWall fw)
       then pure True
       else pure False


-- makeApp :: Env -> IO (Warp.Settings, Application)
makeApp :: FireWall -> IO Application
makeApp fw = do
    let serverApp = appMock

    -- logWare <- mkRequestLogger def { destination = RequestLogger.Logger $ env^.logger }
    let checkOriginAndHost app req resp = do
            blocking <- fireWall req fw
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
    pure $ checkOriginAndHost $ corsMiddleware $ serverApp


---------------------------------------------------------------------
type PortNumber = Int
---------------------------------------------------------------------
-- | API Global

-- | API for serving @swagger.json@
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | API for serving main operational routes of @gargantext.org@
type GargAPI =  "user"  :> Summary "First user endpoint" 
                        :> Roots
       
           :<|> "node"  :> Summary "Node endpoint"
                        :> Capture "id" Int      :> NodeAPI
           
           :<|> "corpus":> Summary "Corpus endpoint"
                        :> Capture "id" Int      :> NodeAPI

           :<|> "nodes" :> Summary "Nodes endpoint"
                        :> ReqBody '[JSON] [Int] :> NodesAPI
       
       -- :<|> "counts" :> Stream GET NewLineFraming '[JSON] Count :> CountAPI
           :<|> "count" :> Summary "Count endpoint"
                        :> ReqBody '[JSON] Query :> CountAPI 

-- /mv/<id>/<id>
-- /merge/<id>/<id>
-- /rename/<id>
       -- :<|> "static"   
       -- :<|> "list"     :> Capture "id" Int  :> NodeAPI
       -- :<|> "ngrams"   :> Capture "id" Int  :> NodeAPI
       -- :<|> "auth"     :> Capture "id" Int  :> NodeAPI
---------------------------------------------------------------------
type SwaggerFrontAPI = SwaggerAPI :<|> FrontEndAPI 

type API = SwaggerFrontAPI :<|> GargAPI

---------------------------------------------------------------------
-- | Server declaration
server :: Connection -> Server API
server conn = swaggerFront
          :<|> roots    conn
          :<|> nodeAPI  conn
          :<|> nodeAPI  conn
          :<|> nodesAPI conn
          :<|> count

---------------------------------------------------------------------
swaggerFront :: Server SwaggerFrontAPI
swaggerFront = schemaUiServer swaggerDoc
           :<|> frontEndServer

gargMock :: Server GargAPI
gargMock = mock apiGarg Proxy

---------------------------------------------------------------------
app :: Connection -> Application
app  = serve api . server

appMock :: Application
appMock = serve api (swaggerFront :<|> gargMock)

---------------------------------------------------------------------
api :: Proxy API
api  = Proxy

apiGarg :: Proxy GargAPI
apiGarg  = Proxy
---------------------------------------------------------------------

schemaUiServer :: (Server api ~ Handler Swagger)
        => Swagger -> Server (SwaggerSchemaUI' dir api)
schemaUiServer = swaggerSchemaUIServer


-- Type Familiy for the Documentation
type family TypeName (x :: *) :: Symbol where
    TypeName Int  = "Int"
    TypeName Text = "Text"
    TypeName x    = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: *) :: Symbol where
    GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))


-- | Swagger Specifications
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy GargAPI)
  & info.title       .~ "Gargantext"
  & info.version     .~ "0.1.0"
  -- & info.base_url     ?~ (URL "http://gargantext.org/")
  & info.description ?~ "REST API specifications"
  -- & tags             .~ Set.fromList [Tag "Garg" (Just "Main perations") Nothing]
  & applyTagsFor (subOperations (Proxy :: Proxy GargAPI)(Proxy :: Proxy GargAPI)) 
                 ["Garg" & description ?~ "Main operations"]
  & info.license     ?~ ("AGPLV3 (English) and CECILL (French)" & url ?~ URL urlLicence )
    where
        urlLicence = "https://gitlab.iscpif.fr/gargantext/haskell-gargantext/blob/master/LICENSE"

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
swaggerWriteJSON :: IO ()
swaggerWriteJSON = BL8.writeFile "swagger.json" (encodePretty swaggerDoc)

portRouteInfo :: PortNumber -> IO ()
portRouteInfo port = do
   print (pack "      ----Main Routes-----      ")
   print      ("http://localhost:" <> show port <> "/index.html")
   print      ("http://localhost:" <> show port <> "/swagger-ui")

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: PortNumber -> FilePath -> IO ()
startGargantext port file = do
  
  param <- databaseParameters file
  conn  <- connect param
  
  portRouteInfo port
  run port (app conn)

startGargantextMock :: PortNumber -> IO ()
startGargantextMock port = do
  portRouteInfo port

  application <- makeApp (FireWall False)

  run port application

