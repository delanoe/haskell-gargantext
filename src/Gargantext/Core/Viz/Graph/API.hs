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

import Control.Lens (set, (^.), _Just, (^?))
import Data.Aeson
import Data.Swagger
import Data.Text
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types (NgramsRepo, r_version)
import Gargantext.API.Prelude
import Gargantext.Core.Methods.Distances (Distance(..), GraphMetric(..))
import Gargantext.Core.Types.Main
import Gargantext.Core.Viz.Graph
import Gargantext.Core.Viz.Graph.GEXF ()
import Gargantext.Core.Viz.Graph.Tools -- (cooc2graph)
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Node (mkNodeWithParent)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.Node.User (getNodeUser)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node (node_parentId, node_hyperdata, node_name, node_userId)
import Gargantext.Prelude
import Servant
import Servant.Job.Async
import Servant.XML
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
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
getGraph :: UserId -> NodeId -> GargNoServer HyperdataGraphAPI
getGraph _uId nId = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)
  let graph  = nodeGraph ^. node_hyperdata . hyperdataGraph
  let camera = nodeGraph ^. node_hyperdata . hyperdataCamera

  repo <- getRepo

  let cId = maybe (panic "[G.V.G.API] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  -- TODO Distance in Graph params
  case graph of
    Nothing     -> do
        graph' <- computeGraph cId Conditional NgramsTerms repo
        mt     <- defaultGraphMetadata cId "Title" repo
        let graph'' = set graph_metadata (Just mt) graph'
        let hg = HyperdataGraphAPI graph'' camera
       -- _      <- updateHyperdata nId hg
        _ <- updateHyperdata nId (HyperdataGraph (Just graph'') camera)
        pure $ trace "[G.V.G.API] Graph empty, computing" hg

    Just graph' -> pure $ trace "[G.V.G.API] Graph exists, returning" $
        HyperdataGraphAPI graph' camera


recomputeGraph :: UserId -> NodeId -> Distance -> GargNoServer Graph
recomputeGraph _uId nId d = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)
  let graph  = nodeGraph ^. node_hyperdata . hyperdataGraph
  let camera = nodeGraph ^. node_hyperdata . hyperdataCamera
  let graphMetadata = graph ^? _Just . graph_metadata . _Just
  let listVersion   = graph ^? _Just . graph_metadata . _Just . gm_list . lfg_version

  repo <- getRepo
  let v   = repo ^. r_version
  let cId = maybe (panic "[G.V.G.API.recomputeGraph] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  case graph of
    Nothing     -> do
      graph' <- computeGraph cId d NgramsTerms repo
      mt     <- defaultGraphMetadata cId "Title" repo
      let graph'' = set graph_metadata (Just mt) graph'
      _ <- updateHyperdata nId (HyperdataGraph (Just graph'') camera)
      pure $ trace "[G.V.G.API.recomputeGraph] Graph empty, computed" graph''

    Just graph' -> if listVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId d NgramsTerms repo
                       let graph''' = set graph_metadata graphMetadata graph''
                       _ <- updateHyperdata nId (HyperdataGraph (Just graph''') camera)
                       pure $ trace "[G.V.G.API] Graph exists, recomputing" graph'''


-- TODO use Database Monad only here ?
computeGraph :: HasNodeError err
             => CorpusId
             -> Distance
             -> NgramsType
             -> NgramsRepo
             -> Cmd err Graph
computeGraph cId d nt repo = do
  lId  <- defaultList cId

  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot MapTerm $ mapTermListRoot [lId] nt repo

  -- TODO split diagonal
  myCooc <- HashMap.filter (>1)
         <$> getCoocByNgrams (Diagonal True)
         <$> groupNodesByNgrams ngs
         <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)

  graph <- liftBase $ cooc2graph d 0 myCooc

  pure graph


defaultGraphMetadata :: HasNodeError err
                     => CorpusId
                     -> Text
                     -> NgramsRepo
                     -> Cmd err GraphMetadata
defaultGraphMetadata cId t repo = do
  lId  <- defaultList cId

  pure $ GraphMetadata {
      _gm_title = t
    , _gm_metric = Order1
    , _gm_corpusId = [cId]
    , _gm_legend = [
          LegendField 1 "#FFF" "Cluster1"
        , LegendField 2 "#FFF" "Cluster2"
        , LegendField 3 "#FFF" "Cluster3"
        , LegendField 4 "#FFF" "Cluster4"
        ]
      , _gm_list = (ListForGraph lId (repo ^. r_version))
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


graphRecompute :: UserId
               -> NodeId
               -> (JobLog -> GargNoServer ())
               -> GargNoServer JobLog
graphRecompute u n logStatus = do
  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _g <- trace (show u) $ recomputeGraph u n Conditional
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
           graphVersions n
      :<|> recomputeVersions u n

graphVersions :: NodeId -> GargNoServer GraphVersions
graphVersions nId = do
  nodeGraph <- getNodeWith nId (Proxy :: Proxy HyperdataGraph)
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
recomputeVersions uId nId = recomputeGraph uId nId Conditional

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
  let uId' = nodeUser ^. node_userId
  nIds <- mkNodeWithParent nodeType (Just pId) uId' $ nodeParent ^. node_name
  case nIds of
    [] -> pure pId
    (nId:_) -> do
      let graphP = graph
      let graphP' = set (graph_metadata . _Just . gm_startForceAtlas) False graphP

      _ <- updateHyperdata nId (HyperdataGraph (Just graphP') camera)

      pure nId

------------------------------------------------------------
getGraphGexf :: UserId
             -> NodeId
             -> GargNoServer (Headers '[Servant.Header "Content-Disposition" Text] Graph)
getGraphGexf uId nId = do
  HyperdataGraphAPI { _hyperdataAPIGraph = graph } <- getGraph uId nId
  pure $ addHeader "attachment; filename=graph.gexf" graph
