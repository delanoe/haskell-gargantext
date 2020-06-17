{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- TODO-SECURITY: Critical

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

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Node
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Swagger
import Data.Text (Text())
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Auth (withAccess, PathId(..))
import Gargantext.API.Prelude
import Gargantext.API.Metrics
import Gargantext.API.Ngrams (TabType(..), TableNgramsApi, apiNgramsTableCorpus, QueryParamR)
import Gargantext.API.Ngrams.NTree (MyTree)
import Gargantext.API.Node.New
import qualified Gargantext.API.Node.Share  as Share
import qualified Gargantext.API.Node.Update as Update

import Gargantext.API.Search (SearchDocsAPI, searchDocs, SearchPairsAPI, searchPairs)
import Gargantext.API.Table
import Gargantext.Core.Types (NodeTableResult)
import Gargantext.Core.Types.Main (Tree, NodeTree, ListType)
import Gargantext.Database.Action.Flow.Pairing (pairing)
import Gargantext.Database.Query.Facet (FacetDoc, OrderBy(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Children (getChildren)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Query.Tree (tree, TreeMode(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude -- (Cmd, CmdM)
import Gargantext.Database.Query.Table.NodeNode
import Gargantext.Prelude
import Gargantext.Viz.Chart
import Gargantext.Viz.Phylo.API (PhyloAPI, phyloAPI)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Gargantext.Database.Query.Table.Node.Update as U (update, Update(..))
import qualified Gargantext.Database.Action.Delete as Action (deleteNode)

{-
import qualified Gargantext.Text.List.Learn as Learn
import qualified Data.Vector as Vec
--}

-- | Admin NodesAPI
-- TODO
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
type Roots =  Get    '[JSON] [Node HyperdataUser]
         :<|> Put    '[JSON] Int -- TODO

-- | TODO: access by admin only
roots :: GargServer Roots
roots = getNodesWithParentId Nothing
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
             :<|> PostNodeAsync
             :<|> ReqBody '[JSON] a :> Put    '[JSON] Int
             :<|> Delete '[JSON] Int
             :<|> "children"  :> ChildrenApi a

             -- TODO gather it
             :<|> "table"      :> TableApi
             :<|> "ngrams"     :> TableNgramsApi

             :<|> "update"     :> Update.API
             :<|> "category"   :> CatApi
             :<|> "search"     :> SearchDocsAPI
             :<|> "share"      :> Share.API

             -- Pairing utilities
             :<|> "pairwith"   :> PairWith
             :<|> "pairs"      :> Pairs
             :<|> "pairing"    :> PairingApi
             :<|> "searchPair" :> SearchPairsAPI

             -- VIZ
             :<|> "metrics"    :> ScatterAPI
             :<|> "chart"      :> ChartApi
             :<|> "pie"        :> PieApi
             :<|> "tree"       :> TreeApi
             :<|> "phylo"      :> PhyloAPI
             -- :<|> "add"       :> NodeAddAPI

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
                 -- :> Get '[JSON] [Node a]
                 :> Get '[JSON] (NodeTableResult a)

------------------------------------------------------------------------
type NodeNodeAPI a = Get '[JSON] (Node a)

nodeNodeAPI :: forall proxy a. (JSONB a, ToJSON a)
            => proxy a
            -> UserId
            -> CorpusId
            -> NodeId
            -> GargServer (NodeNodeAPI a)
nodeNodeAPI p uId cId nId = withAccess (Proxy :: Proxy (NodeNodeAPI a)) Proxy uId (PathNodeNode cId nId) nodeNodeAPI'
  where
    nodeNodeAPI' :: GargServer (NodeNodeAPI a)
    nodeNodeAPI' = getNodeWith nId p

------------------------------------------------------------------------
-- TODO: make the NodeId type indexed by `a`, then we no longer need the proxy.
nodeAPI :: forall proxy a.
       ( JSONB a
       , FromJSON a
       , ToJSON a
       ) => proxy a
         -> UserId
         -> NodeId
         -> GargServer (NodeAPI a)
nodeAPI p uId id' = withAccess (Proxy :: Proxy (NodeAPI a)) Proxy uId (PathNode id') nodeAPI'
  where
    nodeAPI' :: GargServer (NodeAPI a)
    nodeAPI' =  getNodeWith   id' p
           :<|> rename        id'
           :<|> postNode  uId id'
           :<|> postNodeAsyncAPI  uId id'
           :<|> putNode       id'
           :<|> Action.deleteNode (RootId $ NodeId uId) id'
           :<|> getChildren   id' p

           -- TODO gather it
           :<|> tableApi             id'
           :<|> apiNgramsTableCorpus id'
            
           :<|> Update.api  id'
           :<|> catApi      id'
           :<|> searchDocs  id'
           :<|> Share.api   id'
           -- Pairing Tools
           :<|> pairWith    id'
           :<|> pairs       id'
           :<|> getPair     id'
           :<|> searchPairs id'

           :<|> getScatter id'
           :<|> getChart   id'
           :<|> getPie     id'
           :<|> getTree    id'
           :<|> phyloAPI   id' uId
           -- :<|> nodeAddAPI id'
           -- :<|> postUpload id'

------------------------------------------------------------------------
data RenameNode = RenameNode { r_name :: Text }
  deriving (Generic)

-- TODO unPrefix "r_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  RenameNode
instance ToJSON    RenameNode
instance ToSchema  RenameNode
instance Arbitrary RenameNode where
  arbitrary = elements [RenameNode "test"]
------------------------------------------------------------------------
------------------------------------------------------------------------
type CatApi =  Summary " To Categorize NodeNodes: 0 for delete, 1/null neutral, 2 favorite"
            :> ReqBody '[JSON] NodesToCategory
            :> Put     '[JSON] [Int]

data NodesToCategory = NodesToCategory { ntc_nodesId :: [NodeId]
                                       , ntc_category :: Int
                                       }
  deriving (Generic)

-- TODO unPrefix "ntc_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  NodesToCategory
instance ToJSON    NodesToCategory
instance ToSchema  NodesToCategory

catApi :: CorpusId -> GargServer CatApi
catApi = putCat
  where
    putCat :: CorpusId -> NodesToCategory -> Cmd err [Int]
    putCat cId cs' = nodeNodesCategory $ map (\n -> (cId, n, ntc_category cs')) (ntc_nodesId cs')

------------------------------------------------------------------------
-- TODO adapt FacetDoc -> ListDoc (and add type of document as column)
-- Pairing utilities to move elsewhere
type PairingApi = Summary " Pairing API"
              :> QueryParam "view"   TabType
              -- TODO change TabType -> DocType (CorpusId for pairing)
              :> QueryParam "offset" Int
              :> QueryParam "limit"  Int
              :> QueryParam "order"  OrderBy
              :> Get '[JSON] [FacetDoc]

----------
type Pairs    = Summary "List of Pairs"
              :> Get '[JSON] [AnnuaireId]
pairs :: CorpusId -> GargServer Pairs
pairs cId = do
  ns <- getNodeNode cId
  pure $ map _nn_node2_id ns

type PairWith = Summary "Pair a Corpus with an Annuaire"
              :> "annuaire" :> Capture "annuaire_id" AnnuaireId
              :> "list"     :> Capture "list_id"     ListId
              :> Post '[JSON] Int

pairWith :: CorpusId -> GargServer PairWith
pairWith cId aId lId = do
  r <- pairing cId aId lId
  _ <- insertNodeNode [ NodeNode cId aId Nothing Nothing]
  pure r

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

------------------------------------------------------------------------
type TreeAPI   = QueryParams "type" NodeType :> Get '[JSON] (Tree NodeTree)

treeAPI :: NodeId -> GargServer TreeAPI
treeAPI = tree Advanced

------------------------------------------------------------------------
-- | Check if the name is less than 255 char
rename :: NodeId -> RenameNode -> Cmd err [Int]
rename nId (RenameNode name') = U.update (U.Rename nId name')

putNode :: forall err a. (HasNodeError err, JSONB a, ToJSON a)
        => NodeId
        -> a
        -> Cmd err Int
putNode n h = fromIntegral <$> updateHyperdata n h
-------------------------------------------------------------


