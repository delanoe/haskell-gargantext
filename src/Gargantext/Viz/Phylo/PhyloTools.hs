{-|
Module      : Gargantext.Viz.Phylo.PhyloTools
Description : Module dedicated to all the tools needed for making a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Viz.Phylo.PhyloTools where

import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.List (sort)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo

import GHC.IO (FilePath)

import qualified Data.Vector as Vector


---------------------
-- | Foundations | --
---------------------


-- | Is this Ngrams a Foundations Root ?
isRoots :: Ngrams -> Vector Ngrams -> Bool
isRoots n ns = Vector.elem n ns


--------------
-- | Time | --
--------------


-- | Get a regular & ascendante timeScale from a given list of dates
toTimeScale :: [Date] -> Int -> [Date]
toTimeScale dates step = 
    let dates' = sort dates
    in [head' "toTimeScale" dates', ((head' "toTimeScale" dates') + step) .. last' "toTimeScale" dates']