
{-|
Module      : Gargantext.Server
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main REST API of Gargantext (both Server and Client sides)

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Gargantext.Server
--    ( startApp
--    , app
--    )
      where

import Gargantext.Prelude

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import Database.PostgreSQL.Simple (Connection, connect)
import Opaleye
import System.IO (FilePath, putStrLn, readFile, print)
import Data.Text (Text(), pack)
import Gargantext.Types.Main (Node, NodeId)
import Gargantext.Database.Node (getNodesWithParentId, getNode)
import Gargantext.Database.Private (databaseParameters)

-- | TODO, use MOCK feature of Servant to generate fake data (for tests)

type NodeAPI = Get '[JSON] (Node Value)
           :<|> "children" :> Get '[JSON] [Node Value]

type API =  "roots"  :> Get '[JSON] [Node Value]
       :<|> "node"   :> Capture "id" Int            :> NodeAPI
       :<|> "echo"   :> Capture "string" Text     :> Get '[JSON] Text
       :<|> "upload" :> MultipartForm MultipartData :> Post '[JSON] Text

       -- :<|> "node"  :> Capture "id" Int        :> Get '[JSON] Node

server :: Connection -> Server API
server conn
    = liftIO (getNodesWithParentId conn 0)
  :<|> nodeAPI conn
  :<|> echo
  :<|> upload
    where
        echo s = pure s

startGargantext :: FilePath -> IO ()
startGargantext file = do
  
  print ("Starting server on port " <> show port)
  param <- databaseParameters file
  conn  <- connect param
  
  run port $ app conn
    where
        port = 8008

-- | TODO App type, the main monad in which the bot code is written with.
-- Provide config, state, logs and IO
-- type App m a =  ( MonadState AppState m
--                 , MonadReader Conf m
--                 , MonadLog (WithSeverity Doc) m
--                 , MonadIO m) => m a
-- Thanks @yannEsposito for this.
app :: Connection -> Application
app = serve api . server

api :: Proxy API
api = Proxy

nodeAPI :: Connection -> NodeId -> Server NodeAPI
nodeAPI conn id
    =  liftIO (getNode conn id')
  :<|> liftIO (getNodesWithParentId conn id)
    where
        id' = pgInt4 id

-- | Upload files
-- TODO Is it possible to adapt the function according to iValue input ?
upload :: MultipartData -> Handler Text
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " <> show (iName input)
            <> " -> " <> show (iValue input)

    forM_ (files multipartData) $ \file -> do
      content <- readFile (fdFilePath file)
      putStrLn $ "Content of " <> show (fdFileName file)
              <> " at " <> fdFilePath file
      putStrLn content
  pure (pack "Data loaded")
