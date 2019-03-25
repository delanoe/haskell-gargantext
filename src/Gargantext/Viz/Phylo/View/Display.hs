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

import Data.List        (notElem,last,head,union,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!),sortOn,sort,(\\))
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith,fromList,mapKeys,insert)
import Data.Maybe       (isNothing)
import Data.Set         (Set)
import Data.Text        (Text,unwords)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)

import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vector


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
      nested = foldl (\ns' n -> let nId' = getNodeParentId n
                                in map (\n' -> if getNodeId n' == nId'
                                               then n' & phylo_nodeChilds %~ (++ [n])
                                               else n') ns') ns' ns
      --------------------------------------  


-- | To process a DisplayMode to a PhyloView
processDisplay :: DisplayMode -> PhyloView -> PhyloView
processDisplay d v = case d of 
                     Flat   -> v
                     Nested -> let ns  = sortOn getNodeLevel $ v ^. phylo_viewNodes
                                   lvl = getNodeLevel $ head ns
                               in v & phylo_viewNodes .~ toNestedView (filter (\n -> lvl == getNodeLevel n) ns)
                                                                      (filter (\n -> lvl <  getNodeLevel n) ns) 
                     _      -> panic "[ERR][Viz.Phylo.Example.processDisplay] display not found"