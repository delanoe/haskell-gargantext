{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.View.Display
  where

import Control.Lens     hiding (makeLenses, both, Level)

import Data.List        (head,null,(++),sortOn)
import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

-- | To transform a flat Phyloview into a nested Phyloview 
toNestedView :: [PhyloNode] -> [PhyloNode] -> [PhyloNode]
toNestedView ns ns'
  | null ns'  = ns
  | otherwise = toNestedView (filter (\n -> lvl' == getNodeLevel n) nested)
                             (filter (\n -> lvl' <  getNodeLevel n) nested)
    where
      --------------------------------------
      lvl' :: Level
      lvl' = getNodeLevel $ head $ nested
      --------------------------------------
      nested :: [PhyloNode]
      nested = foldl (\ns'' n -> let nIds' = getNodeParentsId n
                                in map (\n' -> if elem (getNodeId n') nIds'
                                               then n' & pn_childs %~ (++ [n])
                                               else n') ns'') ns' ns
      --------------------------------------  


-- | To process a DisplayMode to a PhyloView
processDisplay :: DisplayMode -> PhyloView -> PhyloView
processDisplay d v = case d of 
                     Flat   -> v
                     Nested -> let ns  = sortOn getNodeLevel $ v ^. pv_nodes
                                   lvl = getNodeLevel $ head ns
                               in v & pv_nodes .~ toNestedView (filter (\n -> lvl == getNodeLevel n) ns)
                                                                      (filter (\n -> lvl <  getNodeLevel n) ns) 
                     --_      -> panic "[ERR][Viz.Phylo.Example.processDisplay] display not found"
