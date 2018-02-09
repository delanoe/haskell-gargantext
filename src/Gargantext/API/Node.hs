{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Node API
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Gargantext.API.Node
      where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value())
import Servant
import Servant.Multipart
import System.IO (putStrLn, readFile)
import Data.Text (Text(), pack)

import Database.PostgreSQL.Simple (Connection)

import Gargantext.Prelude
import Gargantext.Types.Main (Node, NodeId)
import Gargantext.Database.Node (getNodesWithParentId, getNode)


-- | Node API Types management
type Roots = Get '[JSON] [Node Value]

type NodeAPI   = Get '[JSON] (Node Value)
             :<|> "children" :> Get '[JSON] [Node Value]
             :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text
           -- :<|> "children" :> QueryParam "type" Text :> Get '[JSON] [Node Value]


-- | Node API functions
roots :: Connection -> Server Roots
roots conn = liftIO (getNodesWithParentId conn 0)

nodeAPI :: Connection -> NodeId -> Server NodeAPI
nodeAPI conn id =  liftIO (getNode              conn id)
              :<|> liftIO (getNodesWithParentId conn id)
              :<|> upload
              :<|> query

query :: Text -> Handler Text
query s = pure s


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

