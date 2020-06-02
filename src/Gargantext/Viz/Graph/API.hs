{-|
Module      : Gargantext.Viz.Graph
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Viz.Graph.API
  where

import Control.Lens (set, (^.), _Just, (^?))
import Data.Aeson
import Data.Maybe (Maybe(..))
import Data.Swagger
import Data.Text
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (NgramsRepo, r_version)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Prelude
import Gargantext.Core.Types.Main
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node (node_parentId, node_hyperdata)
import Gargantext.Prelude
import Gargantext.Viz.Graph
import Gargantext.Viz.Graph.GEXF ()
import Gargantext.Viz.Graph.Tools -- (cooc2graph)
import Servant
import Servant.Job.Async
import Servant.XML
import qualified Data.Map as Map

------------------------------------------------------------------------
-- | There is no Delete specific API for Graph since it can be deleted
-- as simple Node.
type GraphAPI   =  Get  '[JSON] Graph
              :<|> "gexf" :> Get '[XML] (Headers '[Servant.Header "Content-Disposition" Text] Graph)
              :<|> GraphAsyncAPI
              :<|> "versions" :> GraphVersionsAPI

data GraphVersions =
  GraphVersions { gv_graph :: Maybe Int
                , gv_repo :: Int }
   deriving (Show, Generic)

instance ToJSON GraphVersions
instance ToSchema GraphVersions

graphAPI :: UserId -> NodeId -> GargServer GraphAPI
graphAPI u n =  getGraph       u n
         :<|> getGraphGexf     u n
         :<|> graphAsync       u n
         :<|> graphVersionsAPI u n

------------------------------------------------------------------------
getGraph :: UserId -> NodeId -> GargNoServer Graph
getGraph _uId nId = do
  nodeGraph <- getNodeWith nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  repo <- getRepo

  let cId = maybe (panic "[G.V.G.API] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  g <- case graph of
    Nothing     -> do
        graph' <- computeGraph cId NgramsTerms repo
        _      <- updateHyperdata nId (HyperdataGraph $ Just graph')
        pure $ trace "[G.V.G.API] Graph empty, computing" $ graph'

    Just graph' -> pure $ trace "[G.V.G.API] Graph exists, returning" $ graph'

  pure g


recomputeGraph :: UserId -> NodeId -> GargNoServer Graph
recomputeGraph _uId nId = do
  nodeGraph <- getNodeWith nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  let listVersion = graph ^? _Just
                            . graph_metadata
                            . _Just
                            . gm_list
                            . lfg_version

  repo <- getRepo
  let v   = repo ^. r_version
  let cId = maybe (panic "[G.V.G.API.recomputeGraph] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId

  g <- case graph of
    Nothing     -> do
      graph' <- computeGraph cId NgramsTerms repo
      _ <- updateHyperdata nId (HyperdataGraph $ Just graph')
      pure $ trace "[G.V.G.API.recomputeGraph] Graph empty, computed" $ graph'

    Just graph' -> if listVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId NgramsTerms repo
                       _ <- updateHyperdata nId (HyperdataGraph $ Just graph'')
                       pure $ trace "[G.V.G.API] Graph exists, recomputing" $ graph''
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
         <$> getCoocByNgrams (Diagonal True)
         <$> groupNodesByNgrams ngs
         <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)

  graph <- liftBase $ cooc2graph 0 myCooc
  let graph' = set graph_metadata (Just metadata) graph
  pure graph'

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

------------------------------------------------------------
getGraphGexf :: UserId
             -> NodeId
             -> GargNoServer (Headers '[Servant.Header "Content-Disposition" Text] Graph)
getGraphGexf uId nId = do
  graph <- getGraph uId nId
  pure $ addHeader (concat [ "attachment; filename=graph.gexf" ]) graph



