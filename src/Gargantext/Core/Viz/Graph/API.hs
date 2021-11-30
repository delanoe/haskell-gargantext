{-|
Module      : Gargantext.Core.Viz.Graph
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Core.Viz.Graph.API
  where

import Control.Lens (set, (^.), _Just, (^?), at)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Swagger
import Data.Text hiding (head)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Prelude
import Gargantext.Core.Methods.Distances (Distance(..), GraphMetric(..), withMetric)
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Main
import Gargantext.Core.Viz.Graph
import Gargantext.Core.Viz.Graph.GEXF ()
import Gargantext.Core.Viz.Graph.Tools -- (cooc2graph)
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Node (mkNodeWithParent)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.Node.User (getNodeUser)
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude
import Servant
import Servant.Job.Async
import Servant.XML
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------
-- | There is no Delete specific API for Graph since it can be deleted
-- as simple Node.
type GraphAPI   =  Get  '[JSON] HyperdataGraphAPI
              :<|> "async" :> GraphAsyncAPI
              :<|> "clone"
                   :> ReqBody '[JSON] HyperdataGraphAPI
                   :> Post '[JSON] NodeId
              :<|> "gexf" :> Get '[XML] (Headers '[Servant.Header "Content-Disposition" Text] Graph)
              :<|> "versions" :> GraphVersionsAPI

data GraphVersions =
  GraphVersions { gv_graph :: Maybe Int
                , gv_repo :: Int
                }
   deriving (Show, Generic)

instance ToJSON GraphVersions
instance ToSchema GraphVersions

graphAPI :: UserId -> NodeId -> GargServer GraphAPI
graphAPI u n = getGraph         u n
          :<|> graphAsync       u n
          :<|> graphClone       u n
          :<|> getGraphGexf     u n
          :<|> graphVersionsAPI u n

------------------------------------------------------------------------
--getGraph :: UserId -> NodeId -> GargServer HyperdataGraphAPI
getGraph :: FlowCmdM env err m
         => UserId
         -> NodeId
         -> m HyperdataGraphAPI
getGraph _uId nId = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)

  let
    graph  = nodeGraph ^. node_hyperdata . hyperdataGraph
    camera = nodeGraph ^. node_hyperdata . hyperdataCamera
    
  mcId <- getClosestParentIdByType nId NodeCorpus
  let cId = maybe (panic "[G.V.G.API] Node has no parent") identity mcId

  printDebug "[getGraph] getting list for cId" cId
  listId <- defaultList cId
  repo <- getRepo' [listId]

  -- TODO Distance in Graph params
  case graph of
    Nothing     -> do
        let defaultMetric = Order1
        graph' <- computeGraph cId (withMetric defaultMetric) NgramsTerms repo
        mt     <- defaultGraphMetadata cId "Title" repo defaultMetric
        let
          graph'' = set graph_metadata (Just mt) graph'
          hg = HyperdataGraphAPI graph'' camera
       -- _      <- updateHyperdata nId hg
        _ <- updateHyperdata nId (HyperdataGraph (Just graph'') camera)
        pure $ trace "[G.V.G.API] Graph empty, computing" hg

    Just graph' -> pure $ trace "[G.V.G.API] Graph exists, returning" $
        HyperdataGraphAPI graph' camera


--recomputeGraph :: UserId -> NodeId -> Maybe GraphMetric -> GargNoServer Graph
recomputeGraph :: FlowCmdM env err m
               => UserId
               -> NodeId
               -> Maybe GraphMetric
               -> m Graph
recomputeGraph _uId nId maybeDistance = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)
  let
    graph  = nodeGraph ^. node_hyperdata . hyperdataGraph
    camera = nodeGraph ^. node_hyperdata . hyperdataCamera
    graphMetadata = graph ^? _Just . graph_metadata . _Just
    listVersion   = graph ^? _Just . graph_metadata . _Just . gm_list . lfg_version
    graphMetric   = case maybeDistance of
                      Nothing -> graph ^? _Just . graph_metadata . _Just . gm_metric
                      _       -> maybeDistance
    similarity = case graphMetric of
                   Nothing -> withMetric Order1
                   Just m  -> withMetric m

  mcId <- getClosestParentIdByType nId NodeCorpus
  let cId = maybe (panic "[G.V.G.API] Node has no parent") identity mcId

  listId  <- defaultList cId
  repo <- getRepo' [listId]
  let v   = repo ^. unNodeStory . at listId . _Just . a_version

  case graph of
    Nothing     -> do
      graph' <- computeGraph cId similarity NgramsTerms repo
      mt     <- defaultGraphMetadata cId "Title" repo (fromMaybe Order1 maybeDistance)
      let graph'' = set graph_metadata (Just mt) graph'
      _ <- updateHyperdata nId (HyperdataGraph (Just graph'') camera)
      pure $ trace "[G.V.G.API.recomputeGraph] Graph empty, computed" graph''

    Just graph' -> if listVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId similarity NgramsTerms repo
                       let graph''' = set graph_metadata graphMetadata graph''
                       _ <- updateHyperdata nId (HyperdataGraph (Just graph''') camera)
                       pure $ trace "[G.V.G.API] Graph exists, recomputing" graph'''


computeGraph :: FlowCmdM env err m
             => CorpusId
             -> Distance
             -> NgramsType
             -> NodeListStory
             -> m Graph
computeGraph cId d nt repo = do
  lId  <- defaultList cId
  lIds <- selectNodesWithUsername NodeList userMaster

  let ngs = filterListWithRoot MapTerm
          $ mapTermListRoot [lId] nt repo

  myCooc <- HashMap.filter (>1) -- Removing the hapax (ngrams with 1 cooc)
         -- <$> HashMap.filterWithKey (\(x,y) _ -> x /= y)
         -- <$> getCoocByNgrams (if d == Conditional then Diagonal True else Diagonal False)
         <$> getCoocByNgrams (Diagonal True)
         <$> groupNodesByNgrams ngs
         <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)

  -- printDebug "myCooc" myCooc
  -- saveAsFileDebug "debug/my-cooc" myCooc

  listNgrams <- getListNgrams [lId] nt

  -- graph <- liftBase $ cooc2graphWith Bac d 0 myCooc
  graph <- liftBase $ cooc2graphWith Spinglass d 0 myCooc
  -- saveAsFileDebug "debug/graph" graph

  pure $ mergeGraphNgrams graph (Just listNgrams)


defaultGraphMetadata :: HasNodeError err
                     => CorpusId
                     -> Text
                     -> NodeListStory
                     -> GraphMetric
                     -> Cmd err GraphMetadata
defaultGraphMetadata cId t repo gm = do
  lId  <- defaultList cId

  pure $ GraphMetadata {
      _gm_title = t
    , _gm_metric = gm
    , _gm_corpusId = [cId]
    , _gm_legend = [
          LegendField 1 "#FFF" "Cluster1"
        , LegendField 2 "#FFF" "Cluster2"
        , LegendField 3 "#FFF" "Cluster3"
        , LegendField 4 "#FFF" "Cluster4"
        ]
      , _gm_list = (ListForGraph lId (repo ^. unNodeStory . at lId . _Just . a_version))
      , _gm_startForceAtlas = True
    }
                         -- (map (\n -> LegendField n "#FFFFFF" (pack $ show n)) [1..10])

------------------------------------------------------------
type GraphAsyncAPI = Summary "Recompute graph"
                     :> "recompute"
                     :> AsyncJobsAPI JobLog () JobLog


graphAsync :: UserId -> NodeId -> GargServer GraphAsyncAPI
graphAsync u n =
  serveJobsAPI $
    JobFunction (\_ log' -> graphRecompute u n (liftBase . log'))


--graphRecompute :: UserId
--               -> NodeId
--               -> (JobLog -> GargNoServer ())
--               -> GargNoServer JobLog
graphRecompute :: FlowCmdM env err m
               => UserId
               -> NodeId
               -> (JobLog -> m ())
               -> m JobLog
graphRecompute u n logStatus = do
  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _g <- trace (show u) $ recomputeGraph u n Nothing
  pure  JobLog { _scst_succeeded = Just 1
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
           graphVersions 0 n
      :<|> recomputeVersions u n

graphVersions :: Int -> NodeId -> GargNoServer GraphVersions
graphVersions n nId = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)
  let
    graph =  nodeGraph
          ^. node_hyperdata
           . hyperdataGraph

    listVersion =  graph
                ^? _Just
                . graph_metadata
                . _Just
                . gm_list
                . lfg_version

  mcId <- getClosestParentIdByType nId NodeCorpus
  let cId = maybe (panic "[G.V.G.API] Node has no parent") identity mcId

  maybeListId <- defaultListMaybe cId
  case maybeListId of
    Nothing     -> if n <= 2
                      then graphVersions (n+1) cId
                      else panic "[G.V.G.API] list not found after iterations"

    Just listId -> do
      repo <- getRepo' [listId]
      let v = repo ^. unNodeStory . at listId . _Just . a_version
      printDebug "graphVersions" v

      pure $ GraphVersions { gv_graph = listVersion
                           , gv_repo = v }

--recomputeVersions :: UserId -> NodeId -> GargNoServer Graph
recomputeVersions :: FlowCmdM env err m
                  => UserId
                  -> NodeId
                  -> m Graph
recomputeVersions uId nId = recomputeGraph uId nId Nothing

------------------------------------------------------------
graphClone :: UserId
           -> NodeId
           -> HyperdataGraphAPI
           -> GargNoServer NodeId
graphClone uId pId (HyperdataGraphAPI { _hyperdataAPIGraph = graph
                                      , _hyperdataAPICamera = camera }) = do
  let nodeType = NodeGraph
  nodeUser <- getNodeUser (NodeId uId)
  nodeParent <- getNodeWith pId (Proxy :: Proxy HyperdataGraph)
  let uId' = nodeUser ^. node_user_id
  nIds <- mkNodeWithParent nodeType (Just pId) uId' $ nodeParent ^. node_name
  case nIds of
    [] -> pure pId
    (nId:_) -> do
      let graphP = graph
      let graphP' = set (graph_metadata . _Just . gm_startForceAtlas) False graphP

      _ <- updateHyperdata nId (HyperdataGraph (Just graphP') camera)

      pure nId

------------------------------------------------------------
--getGraphGexf :: UserId
--             -> NodeId
--             -> GargNoServer (Headers '[Servant.Header "Content-Disposition" Text] Graph)
getGraphGexf :: FlowCmdM env err m
             => UserId
             -> NodeId
             -> m (Headers '[Servant.Header "Content-Disposition" Text] Graph)
getGraphGexf uId nId = do
  HyperdataGraphAPI { _hyperdataAPIGraph = graph } <- getGraph uId nId
  pure $ addHeader "attachment; filename=graph.gexf" graph

