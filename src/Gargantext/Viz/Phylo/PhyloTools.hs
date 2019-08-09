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

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo

import GHC.IO (FilePath)

import qualified Data.Vector as Vector


----------------
-- | Config | --
----------------

-- | Define a default value
def :: a -> Maybe a -> a
def = fromMaybe


-- | To init a configuration
initConfig :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe CorpusParser -> Maybe Int -> Maybe Text
           -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Bool -> Config
initConfig (def "" -> corpus) (def "" -> mapList) (def "" -> output) (def Csv -> parser) (def 10000 -> limit) (def "A phylomemy" -> name)
           (def 2 -> level) (def 3 -> period) (def 1 -> step) (def 3 -> support) (def 4 -> clique) (def 3 -> minBranchSize) (def True -> safe) =
           Config corpus mapList output parser limit name level period step support clique minBranchSize safe 


---------------------
-- | Foundations | --
---------------------


-- | Is this Ngrams a Foundations Root ?
isRoots :: Ngrams -> Vector Ngrams -> Bool
isRoots n ns = Vector.elem n ns