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

{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

-------------------------------------------------------------------
module Gargantext.API.Node
      where
-------------------------------------------------------------------

import Control.Lens (prism')
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>))
--import System.IO (putStrLn, readFile)

import Data.Aeson (FromJSON, ToJSON, Value())
--import Data.Text (Text(), pack)
import Data.Text (Text())
import Data.Swagger
import Data.Time (UTCTime)

import Database.PostgreSQL.Simple (Connection)

import GHC.Generics (Generic)
import Servant
-- import Servant.Multipart

import Gargantext.Prelude
import Gargantext.Database.Types.Node
import Gargantext.Database.Node ( runCmd
                                , getNodesWithParentId
                                , getNode, getNodesWith
                                , deleteNode, deleteNodes)
import qualified Gargantext.Database.Node.Update as U (update, Update(..))
import Gargantext.Database.Facet (FacetDoc, getDocFacet
                                 ,FacetChart)
import Gargantext.Database.Tree (treeDB, HasTreeError(..), TreeError(..))

-- Graph
import Gargantext.TextFlow
import Gargantext.Viz.Graph (Graph)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types.Main (Tree, NodeTree)
import Gargantext.Text.Terms (TermType(..))

import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
-------------------------------------------------------------------
-- | Node API Types management
type Roots =  Get    '[JSON] [Node Value]
         :<|> Post   '[JSON] Int -- TODO
         :<|> Put    '[JSON] Int -- TODO
         :<|> Delete '[JSON] Int -- TODO

type NodesAPI  = Delete '[JSON] Int



data Rename = Rename { name :: Text }
  deriving (Generic)

instance FromJSON Rename
instance ToJSON Rename
instance ToSchema Rename
instance Arbitrary Rename where
  arbitrary = elements [Rename "test"]

type NodeAPI   = Get '[JSON] (Node Value)
             :<|> "rename" :> Summary " Rename Node" 
                           :> ReqBody '[JSON] Rename
                           :> Put     '[JSON] [Int]
             :<|> Post   '[JSON] Int
             :<|> Put    '[JSON] Int
             :<|> Delete '[JSON] Int
             :<|> "children" :> Summary " Summary children"
                             :> QueryParam "type"   NodeType
                             :> QueryParam "offset" Int
                             :> QueryParam "limit"  Int
                             :> Get '[JSON] [Node Value]
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

                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                
                -- To launch a query and update the corpus
             -- :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text


-- | Node API functions
roots :: Connection -> Server Roots
roots conn = liftIO (putStrLn ( "/user" :: Text) >> getNodesWithParentId 0 Nothing conn)
          :<|> pure (panic "not implemented yet") -- TODO
          :<|> pure (panic "not implemented yet") -- TODO
          :<|> pure (panic "not implemented yet") -- TODO


type GraphAPI   = Get '[JSON] Graph
graphAPI :: Connection -> NodeId -> Server GraphAPI
graphAPI _ _ = liftIO $ textFlow (Mono EN) (Contexts contextText)
  -- TODO what do we get about the node? to replace contextText

-- TODO(orphan): There should be a proper APIError data type with a case TreeError.
instance HasTreeError ServantErr where
  _TreeError = prism' mk (const Nothing) -- Note a prism
    where
      mk NoRoot       = err404 { errBody = "Root node not found"           }
      mk EmptyRoot    = err500 { errBody = "Root node should not be empty" }
      mk TooManyRoots = err500 { errBody = "Too many root nodes"           }

type TreeAPI   = Get '[JSON] (Tree NodeTree)
treeAPI :: Connection -> NodeId -> Server TreeAPI
treeAPI = treeDB

nodeAPI :: Connection -> NodeId -> Server NodeAPI
nodeAPI conn id =  liftIO (putStrLn ("/node" :: Text) >> getNode              conn id )
              :<|> rename        conn id
              :<|> postNode      conn id
              :<|> putNode       conn id
              :<|> deleteNode'   conn id
              :<|> getNodesWith' conn id
              :<|> getFacet      conn id
              :<|> getChart      conn id
              -- :<|> upload
              -- :<|> query
-- | Check if the name is less than 255 char
--rename :: Connection -> NodeId -> Rename -> Server NodeAPI
rename :: Connection -> NodeId -> Rename -> Handler [Int]
rename c nId (Rename name) = liftIO $ U.update (U.Rename nId name) c

nodesAPI :: Connection -> [NodeId] -> Server NodesAPI
nodesAPI conn ids = deleteNodes' conn ids

postNode :: Connection -> NodeId -> Handler Int
postNode = undefined -- TODO

putNode :: Connection -> NodeId -> Handler Int
putNode = undefined -- TODO

deleteNodes' :: Connection -> [NodeId] -> Handler Int
deleteNodes' conn ids = liftIO (runCmd conn $ deleteNodes ids)

deleteNode' :: Connection -> NodeId -> Handler Int
deleteNode' conn id = liftIO (runCmd conn $ deleteNode id)

getNodesWith' :: Connection -> NodeId -> Maybe NodeType -> Maybe Int -> Maybe Int 
                        -> Handler [Node Value]
getNodesWith' conn id nodeType offset limit  = liftIO (getNodesWith conn id nodeType offset limit)


getFacet :: Connection -> NodeId -> Maybe Int -> Maybe Int
                        -> Handler [FacetDoc]
getFacet conn id offset limit = liftIO (putStrLn ( "/facet" :: Text)) >> liftIO (getDocFacet conn NodeCorpus id (Just Document) offset limit)

getChart :: Connection -> NodeId -> Maybe UTCTime -> Maybe UTCTime
                        -> Handler [FacetChart]
getChart _ _ _ _ = undefined -- TODO


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

