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
  , HyperdataAny(..)
  , HyperdataAnnuaire(..)
  , HyperdataCorpus(..)
  , HyperdataResource(..)
  , HyperdataUser(..)
  , HyperdataDocument(..)
  , HyperdataDocumentV3(..)
  ) where
-------------------------------------------------------------------
import Control.Lens (prism')
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>))
--import System.IO (putStrLn, readFile)

import Data.Aeson (FromJSON, ToJSON)
--import Data.Text (Text(), pack)
import Data.Text (Text())
import Data.Swagger
import Data.Time (UTCTime)

import Database.PostgreSQL.Simple (Connection)

import GHC.Generics (Generic)
import Servant

import Gargantext.API.Ngrams (TabType(..), TableNgramsApi, tableNgramsPatch, NgramsIdPatchsFeed, NgramsIdPatchsBack, ListId)
import Gargantext.Prelude
import Gargantext.Database.Types.Node
import Gargantext.Database.Node ( runCmd
                                , getNodesWithParentId
                                , getNode, getNodesWith, CorpusId
                                , deleteNode, deleteNodes, mk, JSONB)
import qualified Gargantext.Database.Node.Update as U (update, Update(..))
import Gargantext.Database.Facet (FacetDoc , runViewDocuments', OrderBy(..)
                                 ,FacetChart)
import Gargantext.Database.Tree (treeDB, HasTreeError(..), TreeError(..))
import Gargantext.Database.NodeNode (nodesToFavorite, nodesToTrash)
-- Graph
import Gargantext.Text.Flow
import Gargantext.Viz.Graph (Graph)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (Offset, Limit)
import Gargantext.Core.Types.Main (Tree, NodeTree)
import Gargantext.Text.Terms (TermType(..))

import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

-------------------------------------------------------------------
-- | TODO : access by admin only
type NodesAPI  = Delete '[JSON] Int

-- | Delete Nodes
-- Be careful: really delete nodes
-- Access by admin only
nodesAPI :: Connection -> [NodeId] -> Server NodesAPI
nodesAPI conn ids = deleteNodes' conn ids

------------------------------------------------------------------------
-- | TODO: access by admin only
-- To manager the Users roots
type Roots =  Get    '[JSON] [NodeAny]
         :<|> Post   '[JSON] Int -- TODO
         :<|> Put    '[JSON] Int -- TODO
         :<|> Delete '[JSON] Int -- TODO

-- | TODO: access by admin only
roots :: Connection -> Server Roots
roots conn = liftIO (putStrLn ( "/user" :: Text) >> getNodesWithParentId 0 Nothing conn)
          :<|> pure (panic "not implemented yet") -- TODO
          :<|> pure (panic "not implemented yet") -- TODO
          :<|> pure (panic "not implemented yet") -- TODO

-------------------------------------------------------------------
-- | Node API Types management
-- TODO : access by users
type NodeAPI a = Get '[JSON] (Node a)
             :<|> "rename" :> RenameApi
             :<|> PostNodeApi
             :<|> Put    '[JSON] Int
             :<|> Delete '[JSON] Int
             :<|> "children"  :> ChildrenApi a
             
             -- TODO gather it
             :<|> "table"     :> TableApi
             :<|> "list"      :> TableNgramsApi
             
             :<|> "chart"     :> ChartApi
             :<|> "favorites" :> FavApi
             :<|> "documents" :> DocsApi

type RenameApi = Summary " RenameNode Node"
               :> ReqBody '[JSON] RenameNode
               :> Put     '[JSON] [Int]

type PostNodeApi = Summary " PostNode Node with ParentId as {id}"
                 :> ReqBody '[JSON] PostNode
                 :> Post    '[JSON] [Int]

type ChildrenApi a = Summary " Summary children"
                 :> QueryParam "type"   NodeType
                 :> QueryParam "offset" Int
                 :> QueryParam "limit"  Int
                 :> Get '[JSON] [Node a]
------------------------------------------------------------------------
-- TODO: make the NodeId type indexed by `a`, then we no longer need the proxy.
nodeAPI :: JSONB a => Connection -> proxy a -> NodeId -> Server (NodeAPI a)
nodeAPI conn p id
                =  liftIO (getNode conn id p)
              :<|> rename        conn id
              :<|> postNode      conn id
              :<|> putNode       conn id
              :<|> deleteNode'   conn id
              :<|> getNodesWith' conn id p
              
              -- TODO gather it
              :<|> getTable      conn id
              :<|> tableNgramsPatch'  conn id
              
              :<|> getChart      conn id
              :<|> favApi        conn id
              :<|> delDocs       conn id
              -- :<|> upload
              -- :<|> query
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
type DocsApi = Summary "Docs : Move to trash"
             :> ReqBody '[JSON] Documents
             :> Delete  '[JSON] [Int]

data Documents = Documents { documents :: [NodeId]}
  deriving (Generic)

instance FromJSON  Documents
instance ToJSON    Documents
instance ToSchema  Documents

delDocs :: Connection -> CorpusId -> Documents -> Handler [Int]
delDocs c cId ds = liftIO $ nodesToTrash c
                $ map (\n -> (cId, n, True)) $ documents ds

------------------------------------------------------------------------
type FavApi =  Summary " Favorites label"
            :> ReqBody '[JSON] Favorites
            :> Put     '[JSON] [Int]
          :<|> Summary " Favorites unlabel"
            :> ReqBody '[JSON] Favorites
            :> Delete  '[JSON] [Int]

data Favorites = Favorites { favorites :: [NodeId]}
  deriving (Generic)

instance FromJSON  Favorites
instance ToJSON    Favorites
instance ToSchema  Favorites

putFav :: Connection -> CorpusId -> Favorites -> Handler [Int]
putFav c cId fs = liftIO $ nodesToFavorite c
                $ map (\n -> (cId, n, True)) $ favorites fs

delFav :: Connection -> CorpusId -> Favorites -> Handler [Int]
delFav c cId fs = liftIO $ nodesToFavorite c
                $ map (\n -> (cId, n, False)) $ favorites fs

favApi :: Connection -> CorpusId -> (Favorites -> Handler [Int])
                               :<|> (Favorites -> Handler [Int])
favApi c cId = putFav c cId :<|> delFav c cId

------------------------------------------------------------------------
type TableApi = Summary " Table API"
              :> QueryParam "view"   TabType
              :> QueryParam "offset" Int
              :> QueryParam "limit"  Int
              :> QueryParam "order"  OrderBy
              :> Get '[JSON] [FacetDoc]

------------------------------------------------------------------------
type ChartApi = Summary " Chart API"
              :> QueryParam "from" UTCTime
              :> QueryParam "to"   UTCTime
              :> Get '[JSON] [FacetChart]

                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                
                -- To launch a query and update the corpus
             -- :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text


------------------------------------------------------------------------
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

------------------------------------------------------------------------
-- | Check if the name is less than 255 char
rename :: Connection -> NodeId -> RenameNode -> Handler [Int]
rename c nId (RenameNode name) = liftIO $ U.update (U.Rename nId name) c

getTable :: Connection -> NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Handler [FacetDoc]
getTable c cId ft o l order = liftIO $ case ft of
                                (Just Docs)  -> runViewDocuments' c cId False o l order
                                (Just Trash) -> runViewDocuments' c cId True  o l order
                                _     -> panic "not implemented"

getChart :: Connection -> NodeId -> Maybe UTCTime -> Maybe UTCTime
                        -> Handler [FacetChart]
getChart _ _ _ _ = undefined -- TODO

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

tableNgramsPatch' :: Connection -> CorpusId -> Maybe ListId -> NgramsIdPatchsFeed -> Handler NgramsIdPatchsBack
tableNgramsPatch' c cId mL ns = liftIO $ tableNgramsPatch c cId mL ns

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

