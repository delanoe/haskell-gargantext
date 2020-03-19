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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}   -- allows to write Text literals
{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Viz.Graph.API
  where

-- import Debug.Trace (trace)
import Control.Concurrent -- (forkIO)
import Control.Lens (set, (^.), _Just, (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe(..))
import Gargantext.API.Ngrams (NgramsRepo, r_version)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Config
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Node.Select
import Gargantext.Database.Schema.Node (getNodeWith, getNodeUser, defaultList, insertGraph, HasNodeError)
import Gargantext.Database.Types.Node hiding (node_id) -- (GraphId, ListId, CorpusId, NodeId)
import Gargantext.Database.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import Gargantext.Viz.Graph
import Gargantext.Viz.Graph.Tools -- (cooc2graph)
import Servant
import qualified Data.Map as Map

------------------------------------------------------------------------

-- | There is no Delete specific API for Graph since it can be deleted
-- as simple Node.
type GraphAPI   =  Get  '[JSON] Graph
              :<|> Post '[JSON] [GraphId]
              :<|> Put  '[JSON] Int


graphAPI :: UserId -> NodeId -> GargServer GraphAPI
graphAPI u n =  getGraph  u n
         :<|> postGraph n
         :<|> putGraph  n

------------------------------------------------------------------------

{- Model to fork Graph Computation
-- This is not really optimized since it increases the need RAM
-- and freezes the whole system
-- This is mainly for documentation (see a better solution in the function below)
-- Each process has to be tailored
getGraph' :: UserId -> NodeId -> GargServer (Get '[JSON] Graph)
getGraph' u n = do
  newGraph  <- liftIO newEmptyMVar
  g  <- getGraph u n
  _  <- liftIO $ forkIO $ putMVar newGraph g
  g' <- liftIO $ takeMVar newGraph
  pure g'
-}
getGraph :: UserId -> NodeId -> GargNoServer Graph
getGraph uId nId = do
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

  newGraph  <- liftIO newEmptyMVar
  g <- case graph of
    Nothing     -> do
      graph' <- inMVarIO $ computeGraph cId NgramsTerms repo
      _ <- insertGraph cId uId' (HyperdataGraph $ Just graph')
      pure graph'

    Just graph' -> if listVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId NgramsTerms repo
                       _ <- updateHyperdata nId (HyperdataGraph $ Just graph'')
                       pure graph''
  _  <- liftIO $ forkIO $ putMVar newGraph g
  g' <- liftIO $ takeMVar newGraph
  pure {- $ trace (show g) $ -} g'


-- TODO use Database Monad only here ?
computeGraph :: HasNodeError err => CorpusId -> NgramsType -> NgramsRepo -> Cmd err Graph
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

  graph <- liftIO $ inMVarIO $ cooc2graph 0 myCooc
  let graph' = set graph_metadata (Just metadata) graph
  pure graph'



postGraph :: NodeId -> GargServer (Post '[JSON] [NodeId])
postGraph = undefined

putGraph :: NodeId -> GargServer (Put '[JSON] Int)
putGraph = undefined

