{-|
Module      : Gargantext.Core.Viz.Graph
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

module Gargantext.Core.Viz.Graph.GEXF
  where

import Gargantext.Prelude
import Gargantext.Core.Viz.Graph
import qualified Data.HashMap.Lazy as HashMap
import qualified Gargantext.Prelude as P
import qualified Gargantext.Core.Viz.Graph as G
import qualified Xmlbf as Xmlbf
import Prelude (error)

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

-- just to be able to derive a client for the entire gargantext API,
-- we however want to avoid sollicitating this instance
instance Xmlbf.FromXml Graph where
  fromXml = error "FromXml Graph: not defined, just a placeholder"
