{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Data.Gargantext.Server
--    ( startApp
--    , app
--    )
      where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart

-- | TODO, use MOCK feature of Servant to generate fake data (for tests)

data FakeNode = FakeNode
  { fakeNodeId        :: Int
  , fakeNodeName      :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''FakeNode)

type API = "nodes"   :> Get '[JSON] [FakeNode]
       :<|> "node"   :> Capture "id" Int            :> Get '[JSON] FakeNode
       :<|> "echo"   :> Capture "string" String     :> Get '[JSON] String
       :<|> "upload" :> MultipartForm MultipartData :> Post '[JSON] String

       -- :<|> "node"  :> Capture "id" Int        :> Get '[JSON] Node

server :: Server API
server = pure fakeNodes
        :<|> fakeNode
        :<|> echo
        :<|> upload
    where
        echo s = pure s


startGargantext :: IO ()
startGargantext = print ("Starting server on port " ++ show port)  >> run port app
    where
        port = 8008

-- | TODO App type, the main monad in which the bot code is written with.
-- Provide config, state, logs and IO
-- type App m a =  ( MonadState AppState m
--                 , MonadReader Conf m
--                 , MonadLog (WithSeverity Doc) m
--                 , MonadIO m) => m a
-- Thanks @yannEsposito for this.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy


fakeNode :: Monad m => Int -> m FakeNode
fakeNode id = pure (fakeNodes !! id)

fakeNodes :: [FakeNode]
fakeNodes = [ FakeNode 1 "Poincare"
            , FakeNode 2 "Grothendieck"
            ]

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
