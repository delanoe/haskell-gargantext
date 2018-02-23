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

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.API
      where

import Gargantext.Prelude

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Mock (mock)
-- import Servant.API.Stream

import Data.Text (pack)
import Database.PostgreSQL.Simple (Connection, connect)
import System.IO (FilePath, print)

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
---------------------------------------------------------------------

-- | Main routes of the API are typed
type API =  "roots"  :> Roots
       
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


-- | Server declaration
server :: Connection -> Server API
server conn =  roots    conn
          :<|> nodeAPI  conn
          :<|> nodesAPI conn
          :<|> count

---------------------------------------------------------------------
---------------------------------------------------------------------
app :: Connection -> Application
app = serve api . server

api :: Proxy API
api = Proxy
---------------------------------------------------------------------
---------------------------------------------------------------------

