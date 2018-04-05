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
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE OverloadedStrings           #-}

-------------------------------------------------------------------
module Gargantext.API.Node
      where
-------------------------------------------------------------------

import System.IO (putStrLn)

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>))
--import System.IO (putStrLn, readFile)

-- import Data.Aeson (Value())
--import Data.Text (Text(), pack)
import Data.Text (Text())
import Data.Time (UTCTime)

import Database.PostgreSQL.Simple (Connection)

import Servant
-- import Servant.Multipart

import Gargantext.Prelude
import Gargantext.Types.Node
import Gargantext.Database.Node ( getNodesWithParentId
                                , getNode, getNodesWith
                                , deleteNode, deleteNodes)
import Gargantext.Database.Facet (FacetDoc, getDocFacet
                                 ,FacetChart)

-------------------------------------------------------------------
-------------------------------------------------------------------
-- | Node API Types management
type Roots =  Get    '[JSON] [Node HyperdataDocument]
         :<|> Post   '[JSON] Int
         :<|> Put    '[JSON] Int
         :<|> Delete '[JSON] Int

type NodesAPI  = Delete '[JSON] Int

type NodeAPI   = Get '[JSON] (Node HyperdataDocument)
             :<|> Delete '[JSON] Int
             :<|> "children" :> Summary " Summary children"
                             :> QueryParam "type"   NodeType
                             :> QueryParam "offset" Int
                             :> QueryParam "limit"  Int
                             :> Get '[JSON] [Node HyperdataDocument]
             :<|> "facet" :> Summary " Facet documents"
                          :> "documents" :> FacetDocAPI
--             :<|> "facet" :<|> "sources"   :<|> FacetSourcesAPI
--             :<|> "facet" :<|> "authors"   :<|> FacetAuthorsAPI
--             :<|> "facet" :<|> "terms"     :<|> FacetTermsAPI

--data FacetFormat = Table | Chart
--data FacetType   = Doc   | Term  | Source | Author
--data Facet       = Facet Doc Format


type FacetDocAPI = "table"
                   :> Summary " Table data"
                   :> QueryParam "offset" Int
                   :> QueryParam "limit"  Int
                   :> Get '[JSON] [FacetDoc]

                :<|> "chart"
                   :> Summary " Chart data"
                   :> QueryParam "from" UTCTime
                   :> QueryParam "to"   UTCTime
                   :> Get '[JSON] [FacetChart]
--
                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                
                -- To launch a query and update the corpus
             -- :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text


-- | Node API functions
roots :: Connection -> Server Roots
roots conn = liftIO (putStrLn "Log Needed" >> getNodesWithParentId conn 0 Nothing)
          :<|> pure (panic "not implemented yet")
          :<|> pure (panic "not implemented yet")
          :<|> pure (panic "not implemented yet")

nodeAPI :: Connection -> NodeId -> Server NodeAPI
nodeAPI conn id =  liftIO (putStrLn "getNode" >> getNode              conn id )
              :<|> deleteNode'   conn id
              :<|> getNodesWith' conn id
              :<|> getFacet      conn id
              :<|> getChart      conn id
              -- :<|> upload
              -- :<|> query

nodesAPI :: Connection -> [NodeId] -> Server NodesAPI
nodesAPI conn ids = deleteNodes' conn ids

deleteNodes' :: Connection -> [NodeId] -> Handler Int
deleteNodes' conn ids = liftIO (deleteNodes conn ids)

deleteNode' :: Connection -> NodeId -> Handler Int
deleteNode' conn id = liftIO (deleteNode conn id)

getNodesWith' :: Connection -> NodeId -> Maybe NodeType -> Maybe Int -> Maybe Int 
                        -> Handler [Node HyperdataDocument]
getNodesWith' conn id nodeType offset limit  = liftIO (getNodesWith conn id nodeType offset limit)


getFacet :: Connection -> NodeId -> Maybe Int -> Maybe Int
                        -> Handler [FacetDoc]
getFacet conn id offset limit = liftIO (getDocFacet conn id (Just Document) offset limit)

getChart :: Connection -> NodeId -> Maybe UTCTime -> Maybe UTCTime
                        -> Handler [FacetChart]
getChart _ _ _ _ = undefined


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

