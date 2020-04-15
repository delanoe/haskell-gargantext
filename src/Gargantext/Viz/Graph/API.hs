{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}   -- allows to write Text literals
{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Viz.Graph.API
  where

import Control.Lens (set, (^.), _Just, (^?))
import Data.Aeson
import Debug.Trace (trace)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Swagger
import Data.Text
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async
import Servant.XML
import qualified Xmlbf as Xmlbf

import Gargantext.API.Ngrams (NgramsRepo, r_version)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Config
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Action.Query.Node.Select
import Gargantext.Database.Action.Query.Node
import Gargantext.Database.Action.Query.Node.User
import Gargantext.Database.Admin.Types.Errors (HasNodeError)
import Gargantext.Database.Admin.Types.Node hiding (node_id) -- (GraphId, ListId, CorpusId, NodeId)
import Gargantext.Database.Action.Query.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Admin.Utils (Cmd)
import Gargantext.Prelude
import qualified Gargantext.Prelude as P
import Gargantext.Viz.Graph
import qualified Gargantext.Viz.Graph as G
import Gargantext.Viz.Graph.Tools -- (cooc2graph)

-- Converts to GEXF format
-- See https://gephi.org/gexf/format/
instance Xmlbf.ToXml Graph where
  toXml (Graph { _graph_nodes = graphNodes
                , _graph_edges = graphEdges }) = root graphNodes graphEdges
    where
      root :: [G.Node] -> [G.Edge] -> [Xmlbf.Node]
      root gn ge =
        Xmlbf.element "gexf" params $ meta <> (graph gn ge)
        where
          params = HashMap.fromList [ ("xmlns", "http://www.gexf.net/1.2draft")
                                    , ("version", "1.2") ]
      meta = Xmlbf.element "meta" params $ creator <> desc
        where
          params = HashMap.fromList [ ("lastmodifieddate", "2020-03-13") ]
      creator = Xmlbf.element "creator" HashMap.empty $ Xmlbf.text "Gargantext.org"
      desc = Xmlbf.element "description" HashMap.empty $ Xmlbf.text "Gargantext gexf file"
      graph :: [G.Node] -> [G.Edge] -> [Xmlbf.Node]
      graph gn ge = Xmlbf.element "graph" params $ (nodes gn) <> (edges ge)
        where
          params = HashMap.fromList [ ("mode", "static")
                                    , ("defaultedgetype", "directed") ]
      nodes :: [G.Node] -> [Xmlbf.Node]
      nodes gn = Xmlbf.element "nodes" HashMap.empty $ P.concatMap node' gn

      node' :: G.Node -> [Xmlbf.Node]
      node' (G.Node { node_id = nId, node_label = l }) =
        Xmlbf.element "node" params []
        where
          params = HashMap.fromList [ ("id", nId)
                                    , ("label", l) ]
      edges :: [G.Edge] -> [Xmlbf.Node]
      edges gn = Xmlbf.element "edges" HashMap.empty $ P.concatMap edge gn
      edge :: G.Edge -> [Xmlbf.Node]
      edge (G.Edge { edge_id = eId, edge_source = es, edge_target = et }) =
        Xmlbf.element "edge" params []
        where
          params = HashMap.fromList [ ("id", eId)
                                    , ("source", es)
                                    , ("target", et) ]

------------------------------------------------------------------------

-- | There is no Delete specific API for Graph since it can be deleted
-- as simple Node.
type GraphAPI   =  Get  '[JSON] Graph
              :<|> Post '[JSON] [GraphId]
              :<|> Put  '[JSON] Int
              :<|> "gexf" :> Get '[XML] (Headers '[Servant.Header "Content-Disposition" Text] Graph)
              :<|> GraphAsyncAPI
              :<|> "versions" :> GraphVersionsAPI
             

data GraphVersions = GraphVersions { gv_graph :: Maybe Int
                                   , gv_repo :: Int } deriving (Show, Generic)

instance ToJSON GraphVersions
instance ToSchema GraphVersions

graphAPI :: UserId -> NodeId -> GargServer GraphAPI
graphAPI u n =  getGraph  u n
         :<|> postGraph n
         :<|> putGraph  n
         :<|> getGraphGexf u n
         :<|> graphAsync u n
         :<|> graphVersionsAPI u n

------------------------------------------------------------------------

getGraph :: UserId -> NodeId -> GargNoServer Graph
getGraph uId nId = do
  nodeGraph <- getNodeWith nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  -- let listVersion = graph ^? _Just
  --                           . graph_metadata
  --                           . _Just
  --                           . gm_list
  --                           . lfg_version

  repo <- getRepo
  -- let v = repo ^. r_version
  nodeUser <- getNodeUser (NodeId uId)

  let uId' = nodeUser ^. node_userId

  let cId = maybe (panic "[ERR:G.V.G.API] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  g <- case graph of
    Nothing     -> do
        graph' <- computeGraph cId NgramsTerms repo
        _ <- insertGraph cId uId' (HyperdataGraph $ Just graph')
        pure $ trace "Graph empty, computing" $ graph'

    Just graph' -> pure $ trace "Graph exists, returning" $ graph'

    -- Just graph' -> if listVersion == Just v
    --                  then pure graph'
    --                  else do
    --                    graph'' <- computeGraph cId NgramsTerms repo
    --                    _ <- updateHyperdata nId (HyperdataGraph $ Just graph'')
    --                    pure graph''
  
  pure g


recomputeGraph :: UserId -> NodeId -> GargNoServer Graph
recomputeGraph uId nId = do
  nodeGraph <- getNodeWith nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  let listVersion = graph ^? _Just
                            . graph_metadata
                            . _Just
                            . gm_list
                            . lfg_version

  repo <- getRepo
  let v = repo ^. r_version
  nodeUser <- getNodeUser (NodeId uId)

  let uId' = nodeUser ^. node_userId

  let cId = maybe (panic "[ERR:G.V.G.API] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  g <- case graph of
    Nothing     -> do
      graph' <- computeGraph cId NgramsTerms repo
      _ <- insertGraph cId uId' (HyperdataGraph $ Just graph')
      pure $ trace "[recomputeGraph] Graph empty, computing" $ graph'

    Just graph' -> if listVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId NgramsTerms repo
                       _ <- updateHyperdata nId (HyperdataGraph $ Just graph'')
                       pure $ trace "[recomputeGraph] Graph exists, recomputing" $ graph''
  pure g


-- TODO use Database Monad only here ?
computeGraph :: HasNodeError err
             => CorpusId
             -> NgramsType
             -> NgramsRepo
             -> Cmd err Graph
computeGraph cId nt repo = do
  lId  <- defaultList cId

  let metadata = GraphMetadata "Title" [cId]
                                     [ LegendField 1 "#FFF" "Cluster"
                                     , LegendField 2 "#FFF" "Cluster"
                                     ]
                                (ListForGraph lId (repo ^. r_version))
                         -- (map (\n -> LegendField n "#FFFFFF" (pack $ show n)) [1..10])

  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot GraphTerm $ mapTermListRoot [lId] nt repo

  myCooc <- Map.filter (>1)
         <$> getCoocByNgrams (Diagonal False)
         <$> groupNodesByNgrams ngs
         <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)

  graph <- liftBase $ cooc2graph 0 myCooc
  let graph' = set graph_metadata (Just metadata) graph
  pure graph'



postGraph :: NodeId -> GargServer (Post '[JSON] [NodeId])
postGraph = undefined

putGraph :: NodeId -> GargServer (Put '[JSON] Int)
putGraph = undefined


------------------------------------------------------------

getGraphGexf :: UserId -> NodeId -> GargNoServer (Headers '[Servant.Header "Content-Disposition" Text] Graph)
getGraphGexf uId nId = do
  graph <- getGraph uId nId
  pure $ addHeader (concat [ "attachment; filename=graph.gexf" ]) graph

------------------------------------------------------------

type GraphAsyncAPI = Summary "Update graph"
                   :> "async"
                   :> AsyncJobsAPI ScraperStatus () ScraperStatus

graphAsync :: UserId -> NodeId -> GargServer GraphAsyncAPI
graphAsync u n =
  serveJobsAPI $
    JobFunction (\_ log' -> graphAsync' u n (liftBase . log'))


graphAsync' :: UserId
           -> NodeId
           -> (ScraperStatus -> GargNoServer ())
           -> GargNoServer ScraperStatus
graphAsync' u n logStatus = do
  logStatus ScraperStatus { _scst_succeeded = Just 0
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 1
                          , _scst_events    = Just []
                          }
  _g <- trace (show u) $ recomputeGraph u n
  pure  ScraperStatus { _scst_succeeded = Just 1
                      , _scst_failed    = Just 0
                      , _scst_remaining = Just 0
                      , _scst_events    = Just []
                      }

------------------------------------------------------------

type GraphVersionsAPI = Summary "Graph versions"
                        :> Get '[JSON] GraphVersions
                   :<|> Summary "Recompute graph version"
                        :> Post '[JSON] Graph

graphVersionsAPI :: UserId -> NodeId -> GargServer GraphVersionsAPI
graphVersionsAPI u n =
           graphVersions u n
      :<|> recomputeVersions u n

graphVersions :: UserId -> NodeId -> GargNoServer GraphVersions
graphVersions _uId nId = do
  nodeGraph <- getNodeWith nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  let listVersion = graph ^? _Just
                            . graph_metadata
                            . _Just
                            . gm_list
                            . lfg_version

  repo <- getRepo
  let v = repo ^. r_version

  pure $ GraphVersions { gv_graph = listVersion
                       , gv_repo = v }

recomputeVersions :: UserId -> NodeId -> GargNoServer Graph
recomputeVersions uId nId = recomputeGraph uId nId
