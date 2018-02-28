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

---------------------------------------------------------------------
module Gargantext.API
      where
---------------------------------------------------------------------
import Gargantext.Prelude

import System.IO (FilePath, print)
import Control.Lens


import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Data.Text (pack)

import Database.PostgreSQL.Simple (Connection, connect)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Mock (mock)
import Servant.Swagger
-- import Servant.API.Stream


-- import Gargantext.API.Auth
import Gargantext.API.Node ( Roots    , roots
                           , NodeAPI  , nodeAPI
                           , NodesAPI , nodesAPI
                           )
import Gargantext.API.Count ( CountAPI, count, Query)
import Gargantext.Database.Utils (databaseParameters)

---------------------------------------------------------------------
---------------------------------------------------------------------
type PortNumber = Int
---------------------------------------------------------------------

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: PortNumber -> FilePath -> IO ()
startGargantext port file = do
  print ("Starting Gargantext server" <> show port)
  print ("http://localhost:" <> show port)
  param <- databaseParameters file
  conn  <- connect param
  run port ( app conn )

startGargantextMock :: PortNumber -> IO ()
startGargantextMock port = do
  print (pack "Starting Mock server")
  print (pack $ "curl "
        <> "-H \"content-type: application/json"
        <> "-d \'{\"query_query\":\"query\"}\'  "
        <> "-v  http://localhost:" 
        <> show port 
        <>"/count"
         )
  run port ( serve api $ mock api Proxy )

---------------------------------------------------------------------
-- | API Global
type API = GargAPI

-- | API for serving main operational routes of @gargantext.org@
type GargAPI =  "roots"  :> Roots
       
           :<|> "node"   :> Capture "id" Int      :> NodeAPI
           :<|> "nodes"  :> ReqBody '[JSON] [Int] :> NodesAPI
       
       -- :<|> "counts" :> Stream GET NewLineFraming '[JSON] Count :> CountAPI
           :<|> "count"  :> ReqBody '[JSON] Query :> CountAPI 

-- /mv/<id>/<id>
-- /merge/<id>/<id>
-- /rename/<id>
       -- :<|> "static"   
       -- :<|> "list"     :> Capture "id" Int  :> NodeAPI
       -- :<|> "ngrams"   :> Capture "id" Int  :> NodeAPI
       -- :<|> "auth"     :> Capture "id" Int  :> NodeAPI
---------------------------------------------------------------------
-- | Server declaration
server :: Connection -> Server API
server conn =  roots    conn
          :<|> nodeAPI  conn
          :<|> nodesAPI conn
          :<|> count

---------------------------------------------------------------------
app :: Connection -> Application
app  = serve api . server

api :: Proxy API
api  = Proxy
---------------------------------------------------------------------

-- | Swagger Specifications
gargSwagger :: Swagger
gargSwagger = toSwagger api
  & info.title   .~ "Gargantext API"
  & info.version .~ "O.1.0"
  & info.description ?~ "This is the main API of Gargantext"
  & info.license ?~ ("AGPL and CECILLv3" & url ?~ URL "https://gitlab.iscpif.fr/gargantext/haskell-gargantext/blob/master/LICENSE")

-- | API for serving @swagger.json@
-- TODO Do we need to add this in the API ?
-- type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty gargSwagger)


