{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Data.Gargantext.Server
--    ( startApp
--    , app
--    )
      where

import Prelude hiding (null)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import Database.PostgreSQL.Simple (Connection, connect)
import Opaleye

import Data.Gargantext.Types.Main (Node, NodeId)
import Data.Gargantext.Database.Node (getNodesWithParentId, getNode)
import Data.Gargantext.Database.Private (infoGargandb)

-- | TODO, use MOCK feature of Servant to generate fake data (for tests)

type NodeAPI = Get '[JSON] (Node Value)
           :<|> "children" :> Get '[JSON] [Node Value]

type API =  "roots"  :> Get '[JSON] [Node Value]
       :<|> "node"   :> Capture "id" Int :> NodeAPI
       :<|> "echo"   :> Capture "string" String     :> Get '[JSON] String
       :<|> "upload" :> MultipartForm MultipartData :> Post '[JSON] String

       -- :<|> "node"  :> Capture "id" Int        :> Get '[JSON] Node

server :: Connection -> Server API
server conn
    = liftIO (getNodesWithParentId conn null)
  :<|> nodeAPI conn
  :<|> echo
  :<|> upload
    where
        echo s = pure s

connectGargandb :: IO Connection
connectGargandb = connect infoGargandb

startGargantext :: IO ()
startGargantext = do
  print ("Starting server on port " ++ show port)
  conn <- connectGargandb
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
  :<|> liftIO (getNodesWithParentId conn (toNullable id'))
       where id' = pgInt4 id

-- | Upload files
-- TODO Is it possible to adapt the function according to iValue input ?
upload :: MultipartData -> Handler String
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      content <- readFile (fdFilePath file)
      putStrLn $ "Content of " ++ show (fdFileName file)
              ++ " at " ++ fdFilePath file
      putStrLn content
  pure "Data loaded"
