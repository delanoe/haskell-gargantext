{-|
Module      : Gargantext.API.Routes
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Gargantext.API.Routes
      where

import Control.Concurrent (threadDelay)
import Control.Lens (view)
import Data.Text (Text)
import Data.Validity
import Servant
import Servant.Auth as SA
import Servant.Auth.Swagger ()
import Servant.Job.Async
import Servant.Swagger.UI

import Gargantext.API.Admin.Auth (withAccess)
import Gargantext.API.Admin.Auth.Types (AuthRequest, AuthResponse, AuthenticatedUser(..), PathId(..))
import Gargantext.API.Admin.FrontEnd (FrontEndAPI)
import Gargantext.API.Context
import Gargantext.API.Count  (CountAPI, count, Query)
import Gargantext.API.Job (jobLogInit)
import Gargantext.API.Ngrams (TableNgramsApi, apiNgramsTableDoc)
import Gargantext.API.Node
import Gargantext.API.Prelude
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Viz.Graph.API
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_max_docs_scrapers)
import qualified Gargantext.API.GraphQL                    as GraphQL
import qualified Gargantext.API.Ngrams.List                as List
import qualified Gargantext.API.Node.Contact               as Contact
import qualified Gargantext.API.Node.Corpus.Annuaire       as Annuaire
import qualified Gargantext.API.Node.Corpus.Export         as CorpusExport
import qualified Gargantext.API.Node.Corpus.Export.Types   as CorpusExport
import qualified Gargantext.API.Node.Corpus.New            as New
import qualified Gargantext.API.Node.Document.Export       as DocumentExport
import qualified Gargantext.API.Node.Document.Export.Types as DocumentExport
import qualified Gargantext.API.Public                     as Public


type GargAPI = "api" :> Summary "API " :> GargAPIVersion
--- | TODO          :<|> Summary "Latest API" :> GargAPI'

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
          :<|> "public"      :> Public.API


type GargPrivateAPI = SA.Auth '[SA.JWT, SA.Cookie] AuthenticatedUser
                    :> GargPrivateAPI'

type GargAdminAPI
              -- Roots endpoint
             =  "user"  :> Summary "First user endpoint"
                        :> Roots
           :<|> "nodes" :> Summary "Nodes endpoint"
                        :> ReqBody '[JSON] [NodeId] :> NodesAPI

type GargPrivateAPI' =
                GargAdminAPI

           -- Node endpoint
           :<|> "node"     :> Summary "Node endpoint"
                           :> Capture "node_id" NodeId
                           :> NodeAPI HyperdataAny

           -- Context endpoint
           :<|> "context"  :> Summary "Node endpoint"
                           :> Capture "node_id" ContextId
                           :> ContextAPI HyperdataAny

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
                           :> CorpusExport.API

           -- Annuaire endpoint
{-
           :<|> "contact"  :> Summary "Contact endpoint"
                           :> Capture "contact_id" ContactId
                           :> NodeAPI HyperdataContact
--}

           :<|> "annuaire" :> Summary "Annuaire endpoint"
                           :> Capture "annuaire_id" AnnuaireId
                           :> NodeAPI HyperdataAnnuaire

           :<|> "annuaire" :> Summary "Contact endpoint"
                           :> Capture "annuaire_id" NodeId
                           :> Contact.API
           -- Document endpoint
           :<|> "document" :> Summary "Document endpoint"
                           :> Capture "doc_id" DocId
                           :> "ngrams"
                           :> TableNgramsApi

           :<|> "texts" :> Capture "node_id" DocId
                           :> DocumentExport.API

        -- :<|> "counts" :> Stream GET NewLineFraming '[JSON] Count :> CountAPI
            -- TODO-SECURITY
           :<|> "count"    :> Summary "Count endpoint"
                           :> ReqBody '[JSON] Query
                           :> CountAPI

           -- Corpus endpoint --> TODO rename s/search/filter/g
           -- :<|> "search"   :> Capture "corpus" NodeId
           --                 :> (Search.API Search.SearchResult)

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
--           :<|> New.AddWithFile
           :<|> New.AddWithQuery

           -- :<|> "annuaire" :> Annuaire.AddWithForm
           -- :<|> New.AddWithFile
       --  :<|> "scraper" :> WithCallbacks ScraperAPI
       --  :<|> "new"  :> New.Api

      -- TODO refactor the 3 routes below
           :<|> List.GETAPI
           :<|> List.JSONAPI
           :<|> List.CSVAPI
{-
           :<|> "wait"   :> Summary "Wait test"
                         :> Capture "x" Int
                         :> WaitAPI -- Get '[JSON] Int
-}
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
       :<|> GraphQL.API
       :<|> FrontEndAPI

-- | API for serving @swagger.json@
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | API for serving main operational routes of @gargantext.org@
-- TODO
-- /mv/<id>/<id>
-- /merge/<id>/<id>
-- /rename/<id>
       -- :<|> "static"
       -- :<|> "list"     :> Capture "node_id" Int  :> NodeAPI
       -- :<|> "ngrams"   :> Capture "node_id" Int  :> NodeAPI
       -- :<|> "auth"     :> Capture "node_id" Int  :> NodeAPI
---------------------------------------------------------------------

---------------------------------------------------------------------
-- | Server declarations

-- TODO-SECURITY admin only: withAdmin
-- Question: How do we mark admins?
serverGargAdminAPI :: GargServer GargAdminAPI
serverGargAdminAPI =  roots
                 :<|> nodesAPI


serverPrivateGargAPI' :: AuthenticatedUser -> GargServer GargPrivateAPI'
serverPrivateGargAPI' (AuthenticatedUser (NodeId uid))
       =  serverGargAdminAPI
     :<|> nodeAPI     (Proxy :: Proxy HyperdataAny)      uid
     :<|> contextAPI  (Proxy :: Proxy HyperdataAny)      uid
     :<|> nodeAPI     (Proxy :: Proxy HyperdataCorpus)   uid
     :<|> nodeNodeAPI (Proxy :: Proxy HyperdataAny)      uid
     :<|> CorpusExport.getCorpus   -- uid
 --    :<|> nodeAPI     (Proxy :: Proxy HyperdataContact)  uid
     :<|> nodeAPI     (Proxy :: Proxy HyperdataAnnuaire) uid
     :<|> Contact.api uid

     :<|> withAccess  (Proxy :: Proxy TableNgramsApi) Proxy uid
          <$> PathNode <*> apiNgramsTableDoc

     :<|> DocumentExport.api uid

     :<|> count -- TODO: undefined

     -- :<|> withAccess (Proxy :: Proxy (Search.API Search.SearchResult)) Proxy uid
     --     <$> PathNode <*> Search.api -- TODO: move elsewhere

     :<|> withAccess (Proxy :: Proxy GraphAPI)       Proxy uid
          <$> PathNode <*> graphAPI uid -- TODO: mock

     :<|> withAccess (Proxy :: Proxy TreeAPI)        Proxy uid
          <$> PathNode <*> treeAPI
     -- TODO access
     :<|> addCorpusWithForm  (RootId (NodeId uid))
    -- :<|> addCorpusWithFile  (RootId (NodeId uid))
     :<|> addCorpusWithQuery (RootId (NodeId uid))

     -- :<|> addAnnuaireWithForm
     -- :<|> New.api  uid -- TODO-SECURITY
     -- :<|> New.info uid -- TODO-SECURITY
     :<|> List.getApi
     :<|> List.jsonApi
     :<|> List.csvApi
--     :<|> waitAPI


----------------------------------------------------------------------
-- For Tests
type WaitAPI = Get '[JSON] Text

waitAPI ::  Int -> GargServer WaitAPI
waitAPI n = do
  let
    m = (10 :: Int) ^ (6 :: Int)
  _ <- liftBase $ threadDelay ( m * n)
  pure $ "Waited: " <> (cs $ show n)
----------------------------------------

addCorpusWithQuery :: User -> GargServer New.AddWithQuery
addCorpusWithQuery user cid =
  serveJobsAPI $
    JobFunction (\q log' -> do
      limit <- view $ hasConfig . gc_max_docs_scrapers
      New.addToCorpusWithQuery user cid q (Just limit) (liftBase . log')
      {- let log' x = do
        printDebug "addToCorpusWithQuery" x
        liftBase $ log x
      -}
      )

{-
addWithFile :: GargServer New.AddWithFile
addWithFile cid i f =
  serveJobsAPI $
    JobFunction (\_i log -> New.addToCorpusWithFile cid i f (liftBase . log))
-}

addCorpusWithForm :: User -> GargServer New.AddWithForm
addCorpusWithForm user cid =
  serveJobsAPI $
    JobFunction (\i log' ->
      let
        log'' x = do
          printDebug "[addToCorpusWithForm] " x
          liftBase $ log' x
      in New.addToCorpusWithForm user cid i log'' (jobLogInit 3))

addCorpusWithFile :: User -> GargServer New.AddWithFile
addCorpusWithFile user cid =
  serveJobsAPI $
    JobFunction (\i log' ->
      let
        log'' x = do
          printDebug "[addToCorpusWithFile]" x
          liftBase $ log' x
      in New.addToCorpusWithFile user cid i log'')

addAnnuaireWithForm :: GargServer Annuaire.AddWithForm
addAnnuaireWithForm cid =
  serveJobsAPI $
    JobFunction (\i log' -> Annuaire.addToAnnuaireWithForm cid i (liftBase . log'))

