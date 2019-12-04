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

import Control.Lens (set, (^.), _Just, (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe(..))
import Gargantext.API.Ngrams (currentVersion)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Config
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Node.Select
import Gargantext.Database.Schema.Node (getNode, defaultList, insertGraph)
import Gargantext.Database.Types.Node hiding (node_id) -- (GraphId, ListId, CorpusId, NodeId)
import Gargantext.Database.Node.UpdateOpaleye (updateHyperdata)
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

getGraph :: UserId -> NodeId -> GargServer (Get '[JSON] Graph)
getGraph uId nId = do
  nodeGraph <- getNode nId HyperdataGraph
  let graph = nodeGraph ^. node_hyperdata . hyperdataGraph
  let graphVersion = graph ^? _Just
                            . graph_metadata
                            . _Just
                            . gm_version

  v <- currentVersion
  nodeUser <- getNode (NodeId uId) HyperdataUser

  let uId' = nodeUser ^. node_userId

  let cId = maybe (panic "[ERR:G.V.G.API] Node has no parent")
                  identity
                  $ nodeGraph ^. node_parentId
  case graph of
    Nothing     -> do
      graph' <- computeGraph cId NgramsTerms v
      _ <- insertGraph cId uId' (HyperdataGraph $ Just graph')
      pure graph'

    Just graph' -> if graphVersion == Just v
                     then pure graph'
                     else do
                       graph'' <- computeGraph cId NgramsTerms v
                       _ <- updateHyperdata nId (HyperdataGraph $ Just graph'')
                       pure graph''

-- TODO use Database Monad only here ?
computeGraph :: CorpusId -> NgramsType -> Int -> GargServer (Get '[JSON] Graph)
computeGraph cId nt v = do
  lId  <- defaultList cId

  let metadata = GraphMetadata "Title" [cId]
                                     [ LegendField 1 "#FFF" "Cluster"
                                     , LegendField 2 "#FFF" "Cluster"
                                     ]
                                lId
                                v
                         -- (map (\n -> LegendField n "#FFFFFF" (pack $ show n)) [1..10])

  lIds <- selectNodesWithUsername NodeList userMaster
  ngs  <- filterListWithRoot GraphTerm <$> mapTermListRoot [lId] nt

  myCooc <- Map.filter (>1) <$> getCoocByNgrams (Diagonal False)
                            <$> groupNodesByNgrams ngs
                            <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)

  graph <- liftIO $ cooc2graph 0 myCooc
  let graph' = set graph_metadata (Just metadata) graph
  pure graph' 



postGraph :: NodeId -> GargServer (Post '[JSON] [NodeId])
postGraph = undefined

putGraph :: NodeId -> GargServer (Put '[JSON] Int)
putGraph = undefined

