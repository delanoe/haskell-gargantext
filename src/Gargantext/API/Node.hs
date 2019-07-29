{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-- TODO-ACCESS: CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
Node API

-------------------------------------------------------------------
-- TODO-ACCESS: access by admin only.
--              At first let's just have an isAdmin check.
--              Later: check userId CanDeleteNodes Nothing
-- TODO-EVENTS: DeletedNodes [NodeId]
--              {"tag": "DeletedNodes", "nodes": [Int*]}


-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Node
  where

import Control.Lens ((.~), (?~))
import Control.Monad ((>>), forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text())
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Metrics
import Gargantext.API.Ngrams (TabType(..), TableNgramsApi, apiNgramsTableCorpus, QueryParamR, TODO)
import Gargantext.API.Ngrams.NTree (MyTree)
import Gargantext.API.Search (SearchDocsAPI, searchDocs, SearchQuery(..))
import Gargantext.API.Types
import Gargantext.Core.Types (Offset, Limit)
import Gargantext.Core.Types.Main (Tree, NodeTree, ListType)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Facet (FacetDoc , runViewDocuments, OrderBy(..),runViewAuthorsDoc)
import Gargantext.Database.Node.Children (getChildren)
import Gargantext.Database.Schema.Node ( getNodesWithParentId, getNode, getNode', deleteNode, deleteNodes, mkNodeWithParent, JSONB, HasNodeError(..))
import Gargantext.Database.Schema.NodeNode (nodeNodesCategory)
import Gargantext.Database.Tree (treeDB)
import Gargantext.Database.Types.Node
import Gargantext.Database.TextSearch
import Gargantext.Database.Utils -- (Cmd, CmdM)
import Gargantext.Database.Learn (FavOrTrash(..), moreLike)
import Gargantext.Prelude
import Gargantext.Prelude.Utils (hash)
import Gargantext.Viz.Chart
import Gargantext.Viz.Phylo.API (PhyloAPI, phyloAPI)
import Servant
import Servant.Multipart
import Servant.Swagger (HasSwagger(toSwagger))
import Servant.Swagger.Internal
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Gargantext.Database.Node.Update as U (update, Update(..))

{-
import qualified Gargantext.Text.List.Learn as Learn
import qualified Data.Vector as Vec
--}


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
             :<|> "ngrams"    :> TableNgramsApi
             :<|> "pairing"   :> PairingApi

             :<|> "category"  :> CatApi
             :<|> "search"    :> SearchDocsAPI

             -- VIZ
             :<|> "metrics" :> ScatterAPI
             :<|> "chart"     :> ChartApi
             :<|> "pie"       :> PieApi
             :<|> "tree"      :> TreeApi
             :<|> "phylo"     :> PhyloAPI
             :<|> "upload"    :> UploadAPI

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
             =  getNode       id p
           :<|> rename        id
           :<|> postNode  uId id
           :<|> putNode       id
           :<|> deleteNodeApi id
           :<|> getChildren   id p

           -- TODO gather it
           :<|> tableApi             id
           :<|> apiNgramsTableCorpus id
           :<|> getPairing           id
           -- :<|> getTableNgramsDoc id
           
           :<|> catApi     id
           
           :<|> searchDocs id
           
           :<|> getScatter id
           :<|> getChart   id
           :<|> getPie     id
           :<|> getTree    id
           :<|> phyloAPI   id uId
           :<|> postUpload id
  where
    deleteNodeApi id' = do
      node <- getNode' id'
      if _node_typename node == nodeTypeId NodeUser
         then panic "not allowed"  -- TODO add proper Right Management Type
         else deleteNode id'
           
           -- Annuaire
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
type CatApi =  Summary " To Categorize NodeNodes: 0 for delete, 1/null neutral, 2 favorite"
            :> ReqBody '[JSON] NodesToCategory
            :> Put     '[JSON] [Int]

data NodesToCategory = NodesToCategory { ntc_nodesId :: [NodeId]
                                       , ntc_category :: Int
                                       }
  deriving (Generic)

instance FromJSON  NodesToCategory
instance ToJSON    NodesToCategory
instance ToSchema  NodesToCategory

catApi :: CorpusId -> GargServer CatApi
catApi = putCat
  where
    putCat :: CorpusId -> NodesToCategory -> Cmd err [Int]
    putCat cId cs' = nodeNodesCategory $ map (\n -> (cId, n, ntc_category cs')) (ntc_nodesId cs')


------------------------------------------------------------------------
type TableApi = Summary " Table API"
              :> ReqBody '[JSON] SearchQuery
              :> QueryParam "view"   TabType
              :> QueryParam "offset" Int
              :> QueryParam "limit"  Int
              :> QueryParam "order"  OrderBy
              :> Post '[JSON] [FacetDoc]

-- TODO adapt FacetDoc -> ListDoc (and add type of document as column)
type PairingApi = Summary " Pairing API"
              :> QueryParam "view"   TabType
              -- TODO change TabType -> DocType (CorpusId for pairing)
              :> QueryParam "offset" Int
              :> QueryParam "limit"  Int
              :> QueryParam "order"  OrderBy
              :> Get '[JSON] [FacetDoc]

------------------------------------------------------------------------
type ChartApi = Summary " Chart API"
              :> QueryParam "from" UTCTime
              :> QueryParam "to"   UTCTime
              :> Get '[JSON] (ChartMetrics Histo)

type PieApi = Summary " Chart API"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParamR "ngramsType" TabType
           :> Get '[JSON] (ChartMetrics Histo)

type TreeApi = Summary " Tree API"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParamR "ngramsType" TabType
           :> QueryParamR "listType"   ListType
           :> Get '[JSON] (ChartMetrics [MyTree])

                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text
                
                -- To launch a query and update the corpus
             -- :<|> "query"    :> Capture "string" Text       :> Get  '[JSON] Text

------------------------------------------------------------------------

{-
NOTE: These instances are not necessary. However, these messages could be part
      of a display function for NodeError/TreeError.
instance HasNodeError ServantErr where
  _NodeError = prism' mk (const Nothing) -- panic "HasNodeError ServantErr: not a prism")
    where
      e = "Gargantext NodeError: "
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

instance HasTreeError ServantErr where
  _TreeError = prism' mk (const Nothing) -- panic "HasTreeError ServantErr: not a prism")
    where
      e = "TreeError: "
      mk NoRoot       = err404 { errBody = e <> "Root node not found"           }
      mk EmptyRoot    = err500 { errBody = e <> "Root node should not be empty" }
      mk TooManyRoots = err500 { errBody = e <> "Too many root nodes"           }
-}

type TreeAPI   = Get '[JSON] (Tree NodeTree)
-- TODO-ACCESS: CanTree or CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
treeAPI :: NodeId -> GargServer TreeAPI
treeAPI = treeDB

------------------------------------------------------------------------
-- | Check if the name is less than 255 char
rename :: NodeId -> RenameNode -> Cmd err [Int]
rename nId (RenameNode name') = U.update (U.Rename nId name')

tableApi :: NodeId -> SearchQuery
         -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
tableApi cId (SearchQuery []) ft o l order = getTable cId ft o l order
tableApi cId (SearchQuery q)  ft o l order = case ft of
      Just Docs  -> searchInCorpus cId q o l order
      Just Trash -> panic "TODO search in Trash" -- TODO searchInCorpus cId q o l order
      _          -> panic "not implemented: search in Fav/Trash/*"

getTable :: NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getTable cId ft o l order =
  case ft of
    (Just Docs)  -> runViewDocuments cId False o l order
    (Just Trash) -> runViewDocuments cId True  o l order
    (Just MoreFav)   -> moreLike cId o l order IsFav
    (Just MoreTrash) -> moreLike cId o l order IsTrash
    _     -> panic "not implemented"

getPairing :: ContactId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getPairing cId ft o l order =
  case ft of
    (Just Docs)  -> runViewAuthorsDoc cId False o l order
    (Just Trash) -> runViewAuthorsDoc cId True  o l order
    _     -> panic "not implemented"

postNode :: HasNodeError err => UserId -> NodeId -> PostNode -> Cmd err [NodeId]
postNode uId pId (PostNode nodeName nt) = mkNodeWithParent nt (Just pId) uId nodeName

putNode :: NodeId -> Cmd err Int
putNode = undefined -- TODO

query :: Monad m => Text -> m Text
query s = pure s

-------------------------------------------------------------
type Hash = Text
data FileType = CSV | PresseRIS
  deriving (Eq, Show, Generic)

instance ToSchema FileType
instance Arbitrary FileType
  where
    arbitrary = elements [CSV, PresseRIS]
instance ToParamSchema FileType

instance ToParamSchema (MultipartData Mem) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance FromHttpApiData FileType
  where
    parseUrlPiece "CSV"       = pure CSV
    parseUrlPiece "PresseRis" = pure PresseRIS
    parseUrlPiece _           = pure CSV -- TODO error here


instance (ToParamSchema a, HasSwagger sub) =>
         HasSwagger (MultipartForm tag a :> sub) where
  -- TODO
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    where
      param = mempty
        & required ?~ True
        & schema   .~ ParamOther sch
      sch = mempty
        & in_         .~ ParamFormData
        & paramSchema .~ toParamSchema (Proxy :: Proxy a)

type UploadAPI = Summary "Upload file(s) to a corpus"
                :> MultipartForm Mem (MultipartData Mem)
                :> QueryParam "fileType"  FileType
                :> Post '[JSON] [Hash]

--postUpload :: NodeId -> Maybe FileType ->  GargServer UploadAPI
--postUpload :: NodeId -> GargServer UploadAPI
postUpload :: NodeId -> MultipartData Mem -> Maybe FileType -> Cmd err [Hash]
postUpload _ _ Nothing = panic "fileType is a required parameter"
postUpload _ multipartData (Just fileType) = do
  putStrLn $ "File Type: " <> (show fileType)
  is <- liftIO $ do
    putStrLn ("Inputs:" :: Text)
    forM (inputs multipartData) $ \input -> do
      putStrLn $ ("iName  " :: Text) <> (iName input)
            <> ("iValue " :: Text) <> (iValue input)
      pure $ iName input

  _ <- forM (files multipartData) $ \file -> do
    let content = fdPayload file
    putStrLn $ ("XXX " :: Text) <> (fdFileName file)
    putStrLn $ ("YYY " :: Text) <>  cs content
    --pure $ cs content
  -- is <- inputs multipartData

  pure $ map (hash . cs) is
