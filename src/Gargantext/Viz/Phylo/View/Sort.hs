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

module Gargantext.Viz.Phylo.View.Sort
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


-- | To sort a PhyloView by Age
sortBranchByAge :: Order -> PhyloView -> PhyloView
sortBranchByAge o v = v & phylo_viewBranches %~ f
  where
    --------------------------------------
    f :: [PhyloBranch] -> [PhyloBranch] 
    f xs = case o of 
           Asc  -> sortOn (getBranchMeta "age") xs
           Desc -> reverse $ sortOn (getBranchMeta "age") xs
    --------------------------------------

-- | To process a Sort to a PhyloView
processSort :: Maybe (Sort,Order) -> Phylo -> PhyloView -> PhyloView 
processSort s p v = case s of
                    Nothing -> v
                    Just s  -> case fst s of
                               ByBranchAge -> sortBranchByAge (snd s) v
                               _           -> panic "[ERR][Viz.Phylo.View.Sort.processSort] sort not found"