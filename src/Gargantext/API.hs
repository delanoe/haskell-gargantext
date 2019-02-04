{-|
Module      : Gargantext.API
Description : REST API declaration
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

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

---------------------------------------------------------------------
module Gargantext.API
      where
---------------------------------------------------------------------

import           System.IO (FilePath)

import           GHC.Generics (D1, Meta (..), Rep)
import           GHC.TypeLits (AppendSymbol, Symbol)

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Data.Text (Text)
import qualified Data.Text.IO as T
--import qualified Data.Set as Set

import           Network.Wai
import           Network.Wai.Handler.Warp hiding (defaultSettings)

import           Servant
import           Servant.HTML.Blaze (HTML)
import           Servant.Mock (mock)
--import           Servant.Job.Server (WithCallbacks)
import           Servant.Static.TH.Internal.Server (fileTreeToServer)
import           Servant.Static.TH.Internal.FileTree (fileTypeToFileTree, FileType(FileTypeFile))
import           Servant.Swagger
import           Servant.Swagger.UI
-- import Servant.API.Stream
import           Text.Blaze.Html (Html)

--import Gargantext.API.Swagger
import Gargantext.Prelude
import Gargantext.API.FrontEnd (FrontEndAPI, frontEndServer)

import Gargantext.API.Auth (AuthRequest, AuthResponse, auth)
import Gargantext.API.Node ( GargServer
                           , Roots    , roots
                           , NodeAPI  , nodeAPI
                           , NodesAPI , nodesAPI
                           , GraphAPI , graphAPI
                           , TreeAPI  , treeAPI
                           , HyperdataAny
                           , HyperdataCorpus
                           , HyperdataAnnuaire
                           )
--import Gargantext.Database.Node.Contact (HyperdataContact)
import Gargantext.Database.Types.Node (NodeId, CorpusId, AnnuaireId)
import Gargantext.API.Count  ( CountAPI, count, Query)
import Gargantext.API.Search ( SearchAPI, search, SearchQuery)
import Gargantext.Database.Facet

--import Gargantext.API.Orchestrator
--import Gargantext.API.Orchestrator.Types

---------------------------------------------------------------------

import GHC.Base (Applicative)
-- import Control.Lens

import Data.List (lookup)
import Data.Text.Encoding (encodeUtf8)

--import Network.Wai (Request, requestHeaders, responseLBS)
import Network.Wai (Request, requestHeaders)
--import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

import Network.Wai.Middleware.RequestLogger
-- import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import Network.HTTP.Types hiding (Query)


import Gargantext.API.Settings

fireWall :: Applicative f => Request -> FireWall -> f Bool
fireWall req fw = do
    let origin = lookup "Origin" (requestHeaders req)
    let host   = lookup "Host"   (requestHeaders req)

    let hostOk   = Just (encodeUtf8 "localhost:3000")
    let originOk = Just (encodeUtf8 "http://localhost:8008")

    if  origin == originOk
       && host == hostOk
       || (not $ unFireWall fw)
       
       then pure True
       else pure False


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



makeDevApp :: Env -> IO Application
makeDevApp env = do
    serverApp <- makeApp env

    -- logWare <- mkRequestLogger def { destination = RequestLogger.Logger $ env^.logger }
    --logWare <- mkRequestLogger def { destination = RequestLogger.Logger "/tmp/logs.txt" }
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
    
    --pure (warpS, logWare $ checkOriginAndHost $ corsMiddleware $ serverApp)
    pure $ logStdoutDev $ corsMiddleware $ serverApp

---------------------------------------------------------------------
-- | API Global

-- | API for serving @swagger.json@
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | API for serving main operational routes of @gargantext.org@


type GargAPI = "api" :> Summary "API " :> GargAPIVersion
-- | TODO          :<|> Summary "Latest API" :> GargAPI'


type GargAPIVersion = "v1.0" :> Summary "v1.0: " :> GargAPI'

type GargAPI' =
           -- Auth endpoint
                "auth"  :> Summary "AUTH API"
                        :> ReqBody '[JSON] AuthRequest
                        :> Post    '[JSON] AuthResponse
          
           -- Roots endpoint
          :<|>  "user"  :> Summary "First user endpoint"
                        :> Roots
           
           -- Node endpoint
           :<|> "node"  :> Summary "Node endpoint"
                        :> Capture "id" NodeId      :> NodeAPI HyperdataAny
           
           -- Corpus endpoint
           :<|> "corpus":> Summary "Corpus endpoint"
                        :> Capture "id" CorpusId      :> NodeAPI HyperdataCorpus

           -- Annuaire endpoint
           :<|> "annuaire":> Summary "Annuaire endpoint"
                          :> Capture "id" AnnuaireId      :> NodeAPI HyperdataAnnuaire

           -- Corpus endpoint
           :<|> "nodes" :> Summary "Nodes endpoint"
                        :> ReqBody '[JSON] [NodeId] :> NodesAPI
       
        -- :<|> "counts" :> Stream GET NewLineFraming '[JSON] Count :> CountAPI
           -- Corpus endpoint
           :<|> "count" :> Summary "Count endpoint"
                        :> ReqBody '[JSON] Query :> CountAPI
           
           -- Corpus endpoint
           :<|> "search":> Summary "Search endpoint"
                        :> ReqBody '[JSON] SearchQuery 
                        :> QueryParam "offset" Int
                        :> QueryParam "limit"  Int
                        :> QueryParam "order"  OrderBy
                        :> SearchAPI

           -- TODO move to NodeAPI?
           :<|> "graph" :> Summary "Graph endpoint"
                        :> Capture "id" NodeId       :> GraphAPI

           -- TODO move to NodeAPI?
           -- Tree endpoint
           :<|> "tree" :> Summary "Tree endpoint"
                       :> Capture "id" NodeId        :> TreeAPI


       --    :<|> "scraper" :> WithCallbacks ScraperAPI

-- /mv/<id>/<id>
-- /merge/<id>/<id>
-- /rename/<id>
       -- :<|> "static"
       -- :<|> "list"     :> Capture "id" Int  :> NodeAPI
       -- :<|> "ngrams"   :> Capture "id" Int  :> NodeAPI
       -- :<|> "auth"     :> Capture "id" Int  :> NodeAPI
---------------------------------------------------------------------
type SwaggerFrontAPI = SwaggerAPI :<|> FrontEndAPI

type API = SwaggerFrontAPI :<|> GargAPI :<|> Get '[HTML] Html

---------------------------------------------------------------------
-- | Server declarations

server :: Env -> IO (Server API)
server env = do
  -- orchestrator <- scrapyOrchestrator env
  pure $  swaggerFront
     :<|> hoistServer (Proxy :: Proxy GargAPI) (`runReaderT` env) serverGargAPI
     :<|> serverIndex

serverGargAPI :: GargServer GargAPI
serverGargAPI -- orchestrator
       =  auth
     :<|> roots
     :<|> nodeAPI  (Proxy :: Proxy HyperdataAny)      fakeUserId
     :<|> nodeAPI  (Proxy :: Proxy HyperdataCorpus)   fakeUserId
     :<|> nodeAPI  (Proxy :: Proxy HyperdataAnnuaire) fakeUserId
     :<|> nodesAPI
     :<|> count -- TODO: undefined
     :<|> search
     :<|> graphAPI -- TODO: mock
     :<|> treeAPI
  --   :<|> orchestrator
  where
    fakeUserId = 1 -- TODO

serverIndex :: Server (Get '[HTML] Html)
serverIndex = $(do (Just s) <- liftIO (fileTypeToFileTree (FileTypeFile "purescript-gargantext/dist/index.html"))
                   fileTreeToServer s)

---------------------------------------------------------------------
swaggerFront :: Server SwaggerFrontAPI
swaggerFront = schemaUiServer swaggerDoc
           :<|> frontEndServer

gargMock :: Server GargAPI
gargMock = mock apiGarg Proxy

---------------------------------------------------------------------
makeApp :: Env -> IO Application
makeApp = fmap (serve api) . server

appMock :: Application
appMock = serve api (swaggerFront :<|> gargMock :<|> serverIndex)

---------------------------------------------------------------------
api :: Proxy API
api  = Proxy

apiGarg :: Proxy GargAPI
apiGarg  = Proxy
---------------------------------------------------------------------

schemaUiServer :: (Server api ~ Handler Swagger)
        => Swagger -> Server (SwaggerSchemaUI' dir api)
schemaUiServer = swaggerSchemaUIServer


-- Type Family for the Documentation
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
  & info.version     .~ "4.0.2" -- TODO same version as Gargantext
  -- & info.base_url     ?~ (URL "http://gargantext.org/")
  & info.description ?~ "REST API specifications"
  -- & tags             .~ Set.fromList [Tag "Garg" (Just "Main perations") Nothing]
  & applyTagsFor (subOperations (Proxy :: Proxy GargAPI)(Proxy :: Proxy GargAPI)) 
                 ["Gargantext" & description ?~ "Main operations"]
  & info.license     ?~ ("AGPLV3 (English) and CECILL (French)" & url ?~ URL urlLicence )
    where
        urlLicence = "https://gitlab.iscpif.fr/gargantext/haskell-gargantext/blob/master/LICENSE"

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
swaggerWriteJSON :: IO ()
swaggerWriteJSON = BL8.writeFile "swagger.json" (encodePretty swaggerDoc)

portRouteInfo :: PortNumber -> IO ()
portRouteInfo port = do
  T.putStrLn "      ----Main Routes-----      "
  T.putStrLn $ "http://localhost:" <> toUrlPiece port <> "/index.html"
  T.putStrLn $ "http://localhost:" <> toUrlPiece port <> "/swagger-ui"

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: PortNumber -> FilePath -> IO ()
startGargantext port file = do
  env <- newEnv port file
  portRouteInfo port
  app <- makeDevApp env
  run port app

startGargantextMock :: PortNumber -> IO ()
startGargantextMock port = do
  portRouteInfo port
  application <- makeMockApp . MockEnv $ FireWall False
  run port application

