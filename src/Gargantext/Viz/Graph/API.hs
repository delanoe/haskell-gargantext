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

{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}   -- allows to write Text literals
{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE DataKinds            #-}

module Gargantext.Viz.Graph.API
  where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (set)
--import Servant.Job.Utils (swaggerOptions)
import Gargantext.Database.Schema.Ngrams
import Gargantext.API.Types
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Node ( getNode)
import Gargantext.Database.Types.Node -- (GraphId, ListId, CorpusId, NodeId)
import Gargantext.Prelude
import Gargantext.API.Ngrams.Tools
import Gargantext.Core.Types.Main
import Gargantext.Viz.Graph.Tools -- (cooc2graph)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Viz.Graph
import Servant
import qualified Data.Map as Map

{-
getgraph :: GraphId -> GraphView
getgraph _GraphId = phyloView
--getgraph :: GraphId -> Maybe PhyloQueryView -> PhyloView
--getgraph _GraphId _phyloQueryView = phyloView

postgraph :: CorpusId -> Maybe ListId -> GraphQueryBuild -> Phylo
postgraph = undefined

putgraph :: GraphId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putgraph = undefined
-}

type GraphAPI   = Get '[JSON] Graph

graphAPI :: NodeId -> GargServer GraphAPI
graphAPI nId = do
  nodeGraph <- getNode nId HyperdataGraph

  let metadata = GraphMetadata "Title" [maybe 0 identity $ _node_parentId nodeGraph]
                                     [ LegendField 1 "#FFF" "Cluster"
                                     , LegendField 2 "#FFF" "Cluster"
                                     ]
                         -- (map (\n -> LegendField n "#FFFFFF" (pack $ show n)) [1..10])
  let cId = maybe (panic "no parentId") identity $ _node_parentId nodeGraph

  lId <- defaultList cId
  ngs    <- filterListWithRoot GraphTerm <$> mapTermListRoot [lId] NgramsTerms

  myCooc <- Map.filter (>1) <$> getCoocByNgrams (Diagonal False)
                            <$> groupNodesByNgrams ngs
                            <$> getNodesByNgramsOnlyUser cId NgramsTerms (Map.keys ngs)

  liftIO $ set graph_metadata (Just metadata) <$> cooc2graph myCooc


-- | Instances

