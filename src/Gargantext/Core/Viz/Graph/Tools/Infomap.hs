module Gargantext.Core.Viz.Graph.Tools.Infomap where

import Data.Map.Strict (Map)
import Graph.Types
import Prelude

import qualified Data.Graph.Infomap          as I
import qualified Data.Graph.Infomap.Internal as I


infomap :: String -> Map (Int, Int) Double -> IO [ClusterNode]
infomap infomapCfg gr = map mkClustNode <$> I.infomap infomapCfg gr

  where mkClustNode (I.CNode nid cid) =
          ClusterNode (fromIntegral nid) (fromIntegral cid)
