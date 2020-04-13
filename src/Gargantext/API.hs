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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

---------------------------------------------------------------------
module Gargantext.API
      where
---------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Lens
import Control.Monad.Except (withExceptT, ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (lookup)
import Data.Swagger
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Validity
import Data.Version (showVersion)
import GHC.Base (Applicative)
import GHC.Generics (D1, Meta (..), Rep)
import GHC.TypeLits (AppendSymbol, Symbol)
import Gargantext.API.Auth (AuthRequest, AuthResponse, AuthenticatedUser(..), AuthContext, auth, withAccess, PathId(..))
import Gargantext.API.Count  ( CountAPI, count, Query)
import Gargantext.API.FrontEnd (FrontEndAPI, frontEndServer)
import Gargantext.API.Ngrams (HasRepo(..), HasRepoSaver(..), saveRepo, TableNgramsApi, apiNgramsTableDoc)
import Gargantext.API.Node
import Gargantext.API.Orchestrator.Types
import Gargantext.API.Search (SearchPairsAPI, searchPairs)
import Gargantext.API.Settings
import Gargantext.API.Types
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Query.Node.Contact (HyperdataContact)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Admin.Types.Node (NodeId, CorpusId, AnnuaireId)
import Gargantext.Database.Admin.Utils (HasConnectionPool)
import Gargantext.Prelude
import Gargantext.Viz.Graph.API
import Network.HTTP.Types hiding (Query)
import Network.Wai
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp hiding (defaultSettings)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Auth as SA
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger ()
import Servant.Job.Async
import Servant.Swagger
import Servant.Swagger.UI
import System.IO (FilePath)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.IO               as T
import qualified Gargantext.API.Annuaire    as Annuaire
import qualified Gargantext.API.Corpus.New  as New
import qualified Gargantext.API.Export      as Export
import qualified Gargantext.API.Ngrams.List as List
import qualified Paths_gargantext as PG -- cabal magic build module

showAsServantErr :: GargError -> ServerError
showAsServantErr (GargServerError err) = err
showAsServantErr a = err500 { errBody = BL8.pack $ show a }

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


makeDevMiddleware :: IO Middleware
makeDevMiddleware = do

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

    --pure (warpS, logWare . checkOriginAndHost . corsMiddleware)
    pure $ logStdoutDev . corsMiddleware

---------------------------------------------------------------------
-- | API Global

-- | API for serving @swagger.json@
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | API for serving main operational routes of @gargantext.org@


type GargAPI = "api" :> Summary "API " :> GargAPIVersion
-- | TODO          :<|> Summary "Latest API" :> GargAPI'


type GargAPIVersion = "v1.0"
                   :> Summary "Garg API Version "
                   :> GargAPI'

type GargVersion = "version"
                 :> Summary "Backend version"
                 :> Get '[JSON] Text

type GargAPI' =
           -- Auth endpoint
                "auth"  :> Summary "AUTH API"
                        :> ReqBody '[JSON] AuthRequest
                        :> Post    '[JSON] AuthResponse
          :<|> GargVersion
                   -- TODO-ACCESS here we want to request a particular header for
           -- auth and capabilities.
          :<|> GargPrivateAPI


type GargPrivateAPI = SA.Auth '[SA.JWT, SA.Cookie] AuthenticatedUser :> GargPrivateAPI'

type GargAdminAPI
              -- Roots endpoint
             =  "user"  :> Summary "First user endpoint"
                        :> Roots
           :<|> "nodes" :> Summary "Nodes endpoint"
                        :> ReqBody '[JSON] [NodeId] :> NodesAPI

----------------------------------------
-- For Tests
type WaitAPI = Get '[JSON] Text

waitAPI ::  Int -> GargServer WaitAPI
waitAPI n = do
  let
    m = (10 :: Int) ^ (6 :: Int)
  _ <- liftBase $ threadDelay ( m * n)
  pure $ "Waited: " <> (cs $ show n)
----------------------------------------


type GargPrivateAPI' =
                GargAdminAPI

           -- Node endpoint
           :<|> "node"     :> Summary "Node endpoint"
                           :> Capture "node_id" NodeId
                           :> NodeAPI HyperdataAny

           -- Corpus endpoints
           :<|> "corpus"   :> Summary "Corpus endpoint"
                           :> Capture "corpus_id" CorpusId
                           :> NodeAPI HyperdataCorpus

           :<|> "corpus"   :> Summary "Corpus endpoint"
                           :> Capture "node1_id" NodeId
                           :> "document"
                           :> Capture "node2_id" NodeId
                           :> NodeNodeAPI HyperdataAny

           :<|> "corpus"   :> Capture "node_id" CorpusId
                           :> Export.API

           -- Annuaire endpoint
           :<|> "annuaire" :> Summary "Annuaire endpoint"
                           :> Capture "annuaire_id" AnnuaireId
                           :> NodeAPI HyperdataAnnuaire

           :<|> "annuaire" :> Summary "Contact endpoint"
                           :> Capture "annuaire_id" NodeId
                           :> "contact"
                           :> Capture "contact_id" NodeId
                           :> NodeNodeAPI HyperdataContact

           -- Document endpoint
           :<|> "document" :> Summary "Document endpoint"
                           :> Capture "doc_id" DocId
                           :> "ngrams" :> TableNgramsApi

        -- :<|> "counts" :> Stream GET NewLineFraming '[JSON] Count :> CountAPI
            -- TODO-SECURITY
           :<|> "count"    :> Summary "Count endpoint"
                           :> ReqBody '[JSON] Query
                           :> CountAPI

           -- Corpus endpoint --> TODO rename s/search/filter/g
           :<|> "search"   :> Capture "corpus" NodeId
                           :> SearchPairsAPI

           -- TODO move to NodeAPI?
           :<|> "graph"    :> Summary "Graph endpoint"
                           :> Capture "graph_id" NodeId
                           :> GraphAPI

           -- TODO move to NodeAPI?
           -- Tree endpoint
           :<|> "tree"    :> Summary "Tree endpoint"
                          :> Capture "tree_id" NodeId
                          :> TreeAPI

           -- :<|> New.Upload
           :<|> New.AddWithForm
           :<|> New.AddWithQuery

           :<|> "annuaire" :> Annuaire.AddWithForm
           -- :<|> New.AddWithFile
       --  :<|> "scraper" :> WithCallbacks ScraperAPI
       --  :<|> "new"  :> New.Api

           :<|> "lists"  :> Summary "List export API"
                         :> Capture "listId" ListId
                         :> List.API

           :<|> "wait"   :> Summary "Wait test"
                         :> Capture "x" Int
                         :> WaitAPI -- Get '[JSON] Int

-- /mv/<id>/<id>
-- /merge/<id>/<id>
-- /rename/<id>
       -- :<|> "static"
       -- :<|> "list"     :> Capture "node_id" Int  :> NodeAPI
       -- :<|> "ngrams"   :> Capture "node_id" Int  :> NodeAPI
       -- :<|> "auth"     :> Capture "node_id" Int  :> NodeAPI
---------------------------------------------------------------------

type API = SwaggerAPI
       :<|> GargAPI
       :<|> FrontEndAPI

-- This is the concrete monad. It needs to be used as little as possible,
-- instead, prefer GargServer, GargServerT, GargServerC.
type GargServerM env err = ReaderT env (ExceptT err IO)

type EnvC env =
  ( HasConnectionPool env
  , HasRepo env
  , HasSettings env
  , HasJobEnv env ScraperStatus ScraperStatus
  )

---------------------------------------------------------------------
-- | Server declarations

server :: forall env. EnvC env => env -> IO (Server API)
server env = do
  -- orchestrator <- scrapyOrchestrator env
  pure $  schemaUiServer swaggerDoc
     :<|> hoistServerWithContext 
            (Proxy :: Proxy GargAPI)
            (Proxy :: Proxy AuthContext)
            transform
            serverGargAPI
     :<|> frontEndServer
  where
    transform :: forall a. GargServerM env GargError a -> Handler a
    transform = Handler . withExceptT showAsServantErr . (`runReaderT` env)

serverGargAPI :: GargServerT env err (GargServerM env err) GargAPI
serverGargAPI -- orchestrator
       =  auth
     :<|> gargVersion
     :<|> serverPrivateGargAPI
  --   :<|> orchestrator
  where

    gargVersion :: GargServer GargVersion
    gargVersion = pure (cs $ showVersion PG.version)

    serverPrivateGargAPI :: GargServerT env err (GargServerM env err) GargPrivateAPI
    serverPrivateGargAPI (Authenticated auser) = serverPrivateGargAPI' auser
    serverPrivateGargAPI _                     = throwAll' (_ServerError # err401)
-- Here throwAll' requires a concrete type for the monad.

-- TODO-SECURITY admin only: withAdmin
-- Question: How do we mark admins?
serverGargAdminAPI :: GargServer GargAdminAPI
serverGargAdminAPI =  roots
                 :<|> nodesAPI


serverPrivateGargAPI' :: AuthenticatedUser -> GargServer GargPrivateAPI'
serverPrivateGargAPI' (AuthenticatedUser (NodeId uid))
       =  serverGargAdminAPI
     :<|> nodeAPI     (Proxy :: Proxy HyperdataAny)      uid
     :<|> nodeAPI     (Proxy :: Proxy HyperdataCorpus)   uid
     :<|> nodeNodeAPI (Proxy :: Proxy HyperdataAny)      uid
     :<|> Export.getCorpus   -- uid
     :<|> nodeAPI     (Proxy :: Proxy HyperdataAnnuaire) uid
     :<|> nodeNodeAPI (Proxy :: Proxy HyperdataContact)  uid

     :<|> withAccess  (Proxy :: Proxy TableNgramsApi) Proxy uid
          <$> PathNode <*> apiNgramsTableDoc

     :<|> count -- TODO: undefined

     :<|> withAccess (Proxy :: Proxy SearchPairsAPI) Proxy uid
          <$> PathNode <*> searchPairs -- TODO: move elsewhere

     :<|> withAccess (Proxy :: Proxy GraphAPI)       Proxy uid
          <$> PathNode <*> graphAPI uid -- TODO: mock

     :<|> withAccess (Proxy :: Proxy TreeAPI)        Proxy uid
          <$> PathNode <*> treeAPI
     -- TODO access
     -- :<|> addUpload
     -- :<|> (\corpus -> addWithQuery corpus :<|> addWithFile corpus)
     :<|> addCorpusWithForm  (UserDBId uid)
     :<|> addCorpusWithQuery (RootId   (NodeId uid))

     :<|> addAnnuaireWithForm
     -- :<|> New.api  uid -- TODO-SECURITY
     -- :<|> New.info uid -- TODO-SECURITY
     :<|> List.api
     :<|> waitAPI


addCorpusWithQuery :: User -> GargServer New.AddWithQuery
addCorpusWithQuery user cid =
  serveJobsAPI $
    JobFunction (\i log -> New.addToCorpusWithQuery user cid i (liftBase . log))

addWithFile :: GargServer New.AddWithFile
addWithFile cid i f =
  serveJobsAPI $
    JobFunction (\_i log -> New.addToCorpusWithFile cid i f (liftBase . log))

addCorpusWithForm :: User -> GargServer New.AddWithForm
addCorpusWithForm user cid =
  serveJobsAPI $
    JobFunction (\i log ->
      let
        log' x = do
          printDebug "addCorpusWithForm" x
          liftBase $ log x
      in New.addToCorpusWithForm user cid i log')

addAnnuaireWithForm :: GargServer Annuaire.AddWithForm
addAnnuaireWithForm cid =
  serveJobsAPI $
    JobFunction (\i log -> Annuaire.addToAnnuaireWithForm cid i (liftBase . log))

{-
serverStatic :: Server (Get '[HTML] Html)
serverStatic = $(do
                  let path = "purescript-gargantext/dist/index.html"
                  Just s <- liftBase (fileTypeToFileTree (FileTypeFile path))
                  fileTreeToServer s
                )
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
  & info.version     .~ "0.0.1.3.1" -- TODO same version as Gargantext
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

stopGargantext :: HasRepoSaver env => env -> IO ()
stopGargantext env = do
  T.putStrLn "----- Stopping gargantext -----"
  runReaderT saveRepo env

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: PortNumber -> FilePath -> IO ()
startGargantext port file = do
  env <- newEnv port file
  portRouteInfo port
  app <- makeApp env
  mid <- makeDevMiddleware
  run port (mid app) `finally` stopGargantext env

{-
startGargantextMock :: PortNumber -> IO ()
startGargantextMock port = do
  portRouteInfo port
  application <- makeMockApp . MockEnv $ FireWall False
  run port application
-}



