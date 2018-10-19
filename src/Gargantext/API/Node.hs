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
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

-------------------------------------------------------------------
module Gargantext.API.Node
  ( module Gargantext.API.Node
  , HyperdataCorpus(..)
  , HyperdataResource(..)
  , HyperdataUser(..)
  , HyperdataDocument(..)
  , HyperdataDocumentV3(..)
  ) where
-------------------------------------------------------------------
import Prelude (Enum, Bounded, minBound, maxBound)
import Control.Lens (prism')
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>))
--import System.IO (putStrLn, readFile)

import Data.Either(Either(Left))
import Data.Aeson (FromJSON, ToJSON, Value())
--import Data.Text (Text(), pack)
import Data.Text (Text())
import Data.Swagger
import Data.Time (UTCTime)

import Database.PostgreSQL.Simple (Connection)

import GHC.Generics (Generic)
import Servant

import Gargantext.Prelude
import Gargantext.Database.Types.Node
import Gargantext.Database.Node ( runCmd
                                , getNodesWithParentId
                                , getNode, getNodesWith
                                , deleteNode, deleteNodes, mk, JSONB)
import qualified Gargantext.Database.Node.Update as U (update, Update(..))
import Gargantext.Database.Facet (FacetDoc , runViewDocuments', OrderBy(..)
                                 ,FacetChart)
import Gargantext.Database.Tree (treeDB, HasTreeError(..), TreeError(..))

-- Graph
import Gargantext.TextFlow
import Gargantext.Viz.Graph (Graph)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (Offset, Limit)
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


------------------------------------------------------------------------
------------------------------------------------------------------------
data RenameNode = RenameNode { r_name :: Text }
  deriving (Generic)

instance FromJSON  RenameNode
instance ToJSON    RenameNode
instance ToSchema  RenameNode
instance Arbitrary RenameNode where
  arbitrary = elements [RenameNode "test"]

------------------------------------------------------------------------

data PostNode = PostNode { pn_name :: Text
                         , pn_typename :: NodeType}
  deriving (Generic)

instance FromJSON  PostNode
instance ToJSON    PostNode
instance ToSchema  PostNode
instance Arbitrary PostNode where
  arbitrary = elements [PostNode "Node test" NodeCorpus]

------------------------------------------------------------------------
------------------------------------------------------------------------
type NodeAPI a = Get '[JSON] (Node a)
             :<|> "rename" :> Summary " RenameNode Node"
                           :> ReqBody '[JSON] RenameNode
                           :> Put     '[JSON] [Int]
             :<|> Summary " PostNode Node with ParentId as {id}"
                           :> ReqBody '[JSON] PostNode
                           :> Post    '[JSON] [Int]
             :<|> Put    '[JSON] Int
             :<|> Delete '[JSON] Int
             :<|> "children" :> Summary " Summary children"
                             :> QueryParam "type"   NodeType
                             :> QueryParam "offset" Int
                             :> QueryParam "limit"  Int
                             :> Get '[JSON] [Node a]
             :<|> Summary " Tabs" :> FacetDocAPI

--data FacetFormat = Table | Chart
data FacetType   = Docs   | Terms  | Sources | Authors | Trash
  deriving (Generic, Enum, Bounded)

instance FromHttpApiData FacetType
  where
    parseUrlPiece "Docs" = pure Docs
    parseUrlPiece "Terms" = pure Terms
    parseUrlPiece "Sources" = pure Sources
    parseUrlPiece "Authors" = pure Authors
    parseUrlPiece "Trash"   = pure Trash
    parseUrlPiece _         = Left "Unexpected value of FacetType"

instance ToParamSchema   FacetType
instance ToJSON    FacetType
instance FromJSON  FacetType
instance ToSchema  FacetType
instance Arbitrary FacetType
  where
    arbitrary = elements [minBound .. maxBound]

type FacetDocAPI = "table"
                   :> Summary " Table data"
                   :> QueryParam "view"   FacetType
                   :> QueryParam "offset" Int
                   :> QueryParam "limit"  Int
                   :> QueryParam "order"  OrderBy
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

-- TODO: make the NodeId type indexed by `a`, then we no longer need the proxy.
nodeAPI :: JSONB a => Connection -> proxy a -> NodeId -> Server (NodeAPI a)
nodeAPI conn p id
                =  liftIO (getNode conn id p)
              :<|> rename        conn id
              :<|> postNode      conn id
              :<|> putNode       conn id
              :<|> deleteNode'   conn id
              :<|> getNodesWith' conn id p
              :<|> getTable      conn id
              :<|> getChart      conn id
              -- :<|> upload
              -- :<|> query
-- | Check if the name is less than 255 char
--rename :: Connection -> NodeId -> Rename -> Server NodeAPI
rename :: Connection -> NodeId -> RenameNode -> Handler [Int]
rename c nId (RenameNode name) = liftIO $ U.update (U.Rename nId name) c

nodesAPI :: Connection -> [NodeId] -> Server NodesAPI
nodesAPI conn ids = deleteNodes' conn ids

getTable :: Connection -> NodeId -> Maybe FacetType -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Handler [FacetDoc]
getTable c cId ft o l order = liftIO $ case ft of
                                (Just Docs)  -> runViewDocuments' c cId False o l order
                                (Just Trash) -> runViewDocuments' c cId True  o l order
                                _     -> panic "not implemented"




postNode :: Connection -> NodeId -> PostNode -> Handler [Int]
postNode c pId (PostNode name nt) = liftIO $ mk c nt (Just pId) name

putNode :: Connection -> NodeId -> Handler Int
putNode = undefined -- TODO

deleteNodes' :: Connection -> [NodeId] -> Handler Int
deleteNodes' conn ids = liftIO (runCmd conn $ deleteNodes ids)

deleteNode' :: Connection -> NodeId -> Handler Int
deleteNode' conn id = liftIO (runCmd conn $ deleteNode id)

getNodesWith' :: JSONB a => Connection -> NodeId -> proxy a -> Maybe NodeType
              -> Maybe Int -> Maybe Int -> Handler [Node a]
getNodesWith' conn id p nodeType offset limit  = liftIO (getNodesWith conn id p nodeType offset limit)


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

