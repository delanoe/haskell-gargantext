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
{-# LANGUAGE RankNTypes         #-}
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
import Control.Lens (prism', set)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>))
--import System.IO (putStrLn, readFile)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text())
import Data.Swagger
import Data.Time (UTCTime)

import GHC.Generics (Generic)
import Servant

import Gargantext.API.Ngrams (TabType(..), TableNgramsApi, TableNgramsApiGet, tableNgramsPatch, getTableNgrams)
import Gargantext.Prelude
import Gargantext.Database.Types.Node
import Gargantext.Database.Utils -- (Cmd, CmdM)
import Gargantext.Database.Schema.Node ( getNodesWithParentId, getNode, deleteNode, deleteNodes, mkNodeWithParent, JSONB, NodeError(..), HasNodeError(..))
import Gargantext.Database.Node.Children (getChildren)
import qualified Gargantext.Database.Node.Update as U (update, Update(..))
import Gargantext.Database.Facet (FacetDoc , runViewDocuments, OrderBy(..),FacetChart,runViewAuthorsDoc)
import Gargantext.Database.Tree (treeDB, HasTreeError(..), TreeError(..))
import Gargantext.Database.Metrics.Count (getCoocByDocDev)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Schema.NodeNode (nodesToFavorite, nodesToTrash)
import Gargantext.API.Search ( SearchAPI, searchIn, SearchInQuery)

-- Graph
import Gargantext.Text.Flow (cooc2graph)
import Gargantext.Viz.Graph hiding (Node)-- (Graph(_graph_metadata),LegendField(..), GraphMetadata(..),readGraphFromJson,defaultGraph)
-- import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (Offset, Limit)
import Gargantext.Core.Types.Main (Tree, NodeTree)
import Gargantext.Database.Types.Node (CorpusId, ContactId)
-- import Gargantext.Text.Terms (TermType(..))

import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

type GargServer api = forall env m. CmdM env ServantErr m => ServerT api m

-------------------------------------------------------------------
-- TODO-ACCESS: access by admin only.
--              At first let's just have an isAdmin check.
--              Later: check userId CanDeleteNodes Nothing
-- TODO-EVENTS: DeletedNodes [NodeId]
--              {"tag": "DeletedNodes", "nodes": [Int*]}
type NodesAPI  = Delete '[JSON] Int

-- | Delete Nodes
-- Be careful: really delete nodes
-- Access by admin only
nodesAPI :: [NodeId] -> GargServer NodesAPI
nodesAPI ids = deleteNodes ids

------------------------------------------------------------------------
-- | TODO-ACCESS: access by admin only.
-- At first let's just have an isAdmin check.
-- Later: CanAccessAnyNode or (CanGetAnyNode, CanPutAnyNode)
-- To manage the Users roots
-- TODO-EVENTS:
--   PutNode ?
-- TODO needs design discussion.
type Roots =  Get    '[JSON] [NodeAny]
         :<|> Put    '[JSON] Int -- TODO

-- | TODO: access by admin only
roots :: GargServer Roots
roots = (liftIO (putStrLn ( "/user" :: Text)) >> getNodesWithParentId 0 Nothing)
   :<|> pure (panic "not implemented yet") -- TODO use patch map to update what we need

-------------------------------------------------------------------
-- | Node API Types management
-- TODO-ACCESS : access by users
-- No ownership check is needed if we strictly follow the capability model.
--
-- CanGetNode (Node, Children, TableApi, TableNgramsApiGet, PairingApi, ChartApi,
--             SearchAPI)
-- CanRenameNode (or part of CanEditNode?)
-- CanCreateChildren (PostNodeApi)
-- CanEditNode / CanPutNode TODO not implemented yet
-- CanDeleteNode
-- CanPatch (TableNgramsApi)
-- CanFavorite
-- CanMoveToTrash
type NodeAPI a = Get '[JSON] (Node a)
             :<|> "rename" :> RenameApi
             :<|> PostNodeApi -- TODO move to children POST
             :<|> Put    '[JSON] Int
             :<|> Delete '[JSON] Int
             :<|> "children"  :> ChildrenApi a

             -- TODO gather it
             :<|> "table"     :> TableApi
             :<|> "list"      :> TableNgramsApi
             :<|> "listGet"   :> TableNgramsApiGet
             :<|> "pairing"   :> PairingApi

             :<|> "chart"     :> ChartApi
             :<|> "favorites" :> FavApi
             :<|> "documents" :> DocsApi
             :<|> "search":> Summary "Node Search"
                        :> ReqBody '[JSON] SearchInQuery
                        :> QueryParam "offset" Int
                        :> QueryParam "limit"  Int
                        :> QueryParam "order"  OrderBy
                        :> SearchAPI

-- TODO-ACCESS: check userId CanRenameNode nodeId
-- TODO-EVENTS: NodeRenamed RenameNode or re-use some more general NodeEdited...
type RenameApi = Summary " Rename Node"
               :> ReqBody '[JSON] RenameNode
               :> Put     '[JSON] [Int]

type PostNodeApi = Summary " PostNode Node with ParentId as {id}"
                 :> ReqBody '[JSON] PostNode
                 :> Post    '[JSON] [NodeId]

type ChildrenApi a = Summary " Summary children"
                 :> QueryParam "type"   NodeType
                 :> QueryParam "offset" Int
                 :> QueryParam "limit"  Int
                 :> Get '[JSON] [Node a]
------------------------------------------------------------------------
-- TODO: make the NodeId type indexed by `a`, then we no longer need the proxy.
nodeAPI :: JSONB a => proxy a -> UserId -> NodeId -> GargServer (NodeAPI a)
nodeAPI p uId id
             =  getNode     id p
           :<|> rename      id
           :<|> postNode    uId id
           :<|> putNode     id
           :<|> deleteNode  id
           :<|> getChildren id p

           -- TODO gather it
           :<|> getTable         id
           :<|> tableNgramsPatch id
           :<|> getTableNgrams   id
           :<|> getPairing       id

           :<|> getChart id
           :<|> favApi   id
           :<|> delDocs  id
           :<|> searchIn id
           -- Annuaire
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

delDocs :: CorpusId -> Documents -> Cmd err [Int]
delDocs cId ds = nodesToTrash $ map (\n -> (cId, n, True)) $ documents ds

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

putFav :: CorpusId -> Favorites -> Cmd err [Int]
putFav cId fs = nodesToFavorite $ map (\n -> (cId, n, True)) $ favorites fs

delFav :: CorpusId -> Favorites -> Cmd err [Int]
delFav cId fs = nodesToFavorite $ map (\n -> (cId, n, False)) $ favorites fs

favApi :: CorpusId -> GargServer FavApi
favApi cId = putFav cId :<|> delFav cId

------------------------------------------------------------------------
type TableApi = Summary " Table API"
              :> QueryParam "view"   TabType
              :> QueryParam "offset" Int
              :> QueryParam "limit"  Int
              :> QueryParam "order"  OrderBy
              :> Get '[JSON] [FacetDoc]

-- TODO adapt FacetDoc -> ListDoc (and add type of document as column)
type PairingApi = Summary " Pairing API"
              :> QueryParam "view"   TabType -- TODO change TabType -> DocType (CorpusId for pairing)
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
-- TODO-ACCESS: CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
type GraphAPI   = Get '[JSON] Graph

graphAPI :: NodeId -> GargServer GraphAPI
graphAPI nId = do

  nodeGraph <- getNode nId HyperdataGraph

  let title = "IMT - Scientific publications - 1982-2017 - English"
  let metadata = GraphMetadata title [maybe 0 identity $ _node_parentId nodeGraph]
                                     [ LegendField 6 "#FFF" "Data processing"
                                     , LegendField 7 "#FFF" "Networks"
                                     , LegendField 1 "#FFF" "Material science"
                                     , LegendField 5 "#FFF" "Energy / Environment"
                                     ]
                         -- (map (\n -> LegendField n "#FFFFFF" (pack $ show n)) [1..10])
  let cId = maybe (panic "no parentId") identity $ _node_parentId nodeGraph
  lId <- defaultList cId
  myCooc <- getCoocByDocDev cId lId
  liftIO $ set graph_metadata (Just metadata)
        <$> cooc2graph myCooc
        
        -- <$> maybe defaultGraph identity
        -- <$> readGraphFromJson "purescript-gargantext/dist/examples/imtNew.json"
  -- t <- textFlow (Mono EN) (Contexts contextText)
  -- liftIO $ liftIO $ pure $  maybe t identity maybeGraph
  -- TODO what do we get about the node? to replace contextText

instance HasNodeError ServantErr where
  _NodeError = prism' mk (const Nothing) -- $ panic "HasNodeError ServantErr: not a prism")
    where
      e = "NodeError: "
      mk NoListFound   = err404 { errBody = e <> "No list found"         }
      mk NoRootFound   = err404 { errBody = e <> "No Root found"         }
      mk NoCorpusFound = err404 { errBody = e <> "No Corpus found"       }
      mk NoUserFound   = err404 { errBody = e <> "No User found"         }

      mk MkNode        = err500 { errBody = e <> "Cannot mk node"        }
      mk NegativeId    = err500 { errBody = e <> "Node with negative Id" }
      mk UserNoParent  = err500 { errBody = e <> "Should not have parent"}
      mk HasParent     = err500 { errBody = e <> "NodeType has parent"   }
      mk NotImplYet    = err500 { errBody = e <> "Not implemented yet"   }
      mk ManyParents   = err500 { errBody = e <> "Too many parents"      }
      mk ManyNodeUsers = err500 { errBody = e <> "Many userNode/user"    }

-- TODO(orphan): There should be a proper APIError data type with a case TreeError.
instance HasTreeError ServantErr where
  _TreeError = prism' mk (const Nothing) -- $ panic "HasTreeError ServantErr: not a prism")
    where
      e = "TreeError: "
      mk NoRoot       = err404 { errBody = e <> "Root node not found"           }
      mk EmptyRoot    = err500 { errBody = e <> "Root node should not be empty" }
      mk TooManyRoots = err500 { errBody = e <> "Too many root nodes"           }

type TreeAPI   = Get '[JSON] (Tree NodeTree)
-- TODO-ACCESS: CanTree or CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
treeAPI :: NodeId -> GargServer TreeAPI
treeAPI = treeDB

------------------------------------------------------------------------
-- | Check if the name is less than 255 char
rename :: NodeId -> RenameNode -> Cmd err [Int]
rename nId (RenameNode name) = U.update (U.Rename nId name)

getTable :: NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getTable cId ft o l order = case ft of
                                (Just Docs)  -> runViewDocuments cId False o l order
                                (Just Trash) -> runViewDocuments cId True  o l order
                                _     -> panic "not implemented"

getPairing :: ContactId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getPairing cId ft o l order = case ft of
                                (Just Docs)  -> runViewAuthorsDoc cId False o l order
                                (Just Trash) -> runViewAuthorsDoc cId True  o l order
                                _     -> panic "not implemented"


getChart :: NodeId -> Maybe UTCTime -> Maybe UTCTime
                   -> Cmd err [FacetChart]
getChart _ _ _ = undefined -- TODO

postNode :: HasNodeError err => UserId -> NodeId -> PostNode -> Cmd err [NodeId]
postNode uId pId (PostNode name nt) = mkNodeWithParent nt (Just pId) uId name

putNode :: NodeId -> Cmd err Int
putNode = undefined -- TODO

query :: Monad m => Text -> m Text
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

