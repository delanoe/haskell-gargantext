{-|
Module      : Gargantext.Core.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Core.Viz.Phylo.View.Display
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (null,(++),sortOn)
import Gargantext.Prelude
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.Tools

-- | To transform a flat Phyloview into a nested Phyloview 
toNestedView :: [PhyloNode] -> [PhyloNode] -> [PhyloNode]
toNestedView ns ns'
  | null ns'  = ns
  | otherwise = toNestedView (filter (\n -> lvl' == getNodeLevel n) nested)
                             (filter (\n -> lvl' <  getNodeLevel n) nested)
    where
      --------------------------------------
      lvl' :: Level
      lvl' = getNodeLevel $ head' "toNestedView" nested
      --------------------------------------
      nested :: [PhyloNode]
      nested = foldl (\ns'' n -> let nIds' = getNodeParentsId n
                                in map (\n' -> if elem (getNodeId n') nIds'
                                               then n' & pn_childs %~ (++ [n])
                                               else n') ns'') ns' ns
      --------------------------------------  


-- | To process a DisplayMode to a PhyloView
processDisplay :: DisplayMode -> ExportMode -> PhyloView -> PhyloView
processDisplay d e v = case e of 
                       Json -> case d of 
                               Flat   -> v
                               Nested -> let ns  = sortOn getNodeLevel $ v ^. pv_nodes
                                             lvl = getNodeLevel $ head' "processDisplay" ns
                                         in v & pv_nodes .~ toNestedView (filter (\n -> lvl == getNodeLevel n) ns)
                                                                         (filter (\n -> lvl <  getNodeLevel n) ns)
                       _    -> v  
