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

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value())
import Servant
-- import Servant.Multipart
--import System.IO (putStrLn, readFile)
import Data.Text (Text())
--import Data.Text (Text(), pack)
import Database.PostgreSQL.Simple (Connection)
import Gargantext.Prelude
import Gargantext.Types.Main (Node, NodeId, NodeType)
import Gargantext.Database.Node (getNodesWithParentId
                                , getNode, getNodesWith
                                , deleteNode, deleteNodes)
import Gargantext.Database.Facet (FacetDoc, getDocFacet)


-- | Node API Types management
type Roots = Get '[JSON] [Node Value]

type NodesAPI  = Delete '[JSON] Int

type NodeAPI   = Get '[JSON] (Node Value)
             :<|> Delete '[JSON] Int

             :<|> "children" :> QueryParam "type"   NodeType
                             :> QueryParam "offset" Int
                             :> QueryParam "limit"  Int
                             :> Get '[JSON] [Node Value]


             :<|> "facet" :> QueryParam "type"   NodeType
                          :> QueryParam "offset" Int
                          :> QueryParam "limit"  Int
                          :> Get '[JSON] [FacetDoc]


                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                
                -- To launch a query and update the corpus
             :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text



-- | Node API functions
roots :: Connection -> Server Roots
roots conn = liftIO (getNodesWithParentId conn 0 Nothing)

nodeAPI :: Connection -> NodeId -> Server NodeAPI
nodeAPI conn id =  liftIO (getNode              conn id)
              :<|> deleteNode'   conn id
              :<|> getNodesWith' conn id
              :<|> getDocFacet'  conn id
              -- :<|> upload
              :<|> query

nodesAPI :: Connection -> [NodeId] -> Server NodesAPI
nodesAPI conn ids = deleteNodes' conn ids

deleteNodes' :: Connection -> [NodeId] -> Handler Int
deleteNodes' conn ids = liftIO (deleteNodes conn ids)

deleteNode' :: Connection -> NodeId -> Handler Int
deleteNode' conn id = liftIO (deleteNode conn id)

getNodesWith' :: Connection -> NodeId -> Maybe NodeType -> Maybe Int -> Maybe Int 
                        -> Handler [Node Value]
getNodesWith' conn id nodeType offset limit  = liftIO (getNodesWith conn id nodeType offset limit)

getDocFacet' :: Connection -> NodeId -> Maybe NodeType -> Maybe Int -> Maybe Int
                        -> Handler [FacetDoc]
getDocFacet' conn id nodeType offset limit = liftIO (getDocFacet conn id nodeType offset limit)

query :: Text -> Handler Text
query s = pure s


-- | Upload files
-- TODO Is it possible to adapt the function according to iValue input ?
--upload :: MultipartData -> Handler Text
--upload multipartData = do
--  liftIO $ do
--    putStrLn "Inputs:"
--    forM_ (inputs multipartData) $ \input ->
--      putStrLn $ "  " <> show (iName input)
--            <> " -> " <> show (iValue input)
--
--    forM_ (files multipartData) $ \file -> do
--      content <- readFile (fdFilePath file)
--      putStrLn $ "Content of " <> show (fdFileName file)
--              <> " at " <> fdFilePath file
--      putStrLn content
--  pure (pack "Data loaded")

