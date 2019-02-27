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

module Gargantext.Viz.Phylo.Tools
  where

import Control.Lens hiding (both)
import Data.Tuple.Extra
import Gargantext.Prelude hiding (head)
import Gargantext.Viz.Phylo
import qualified Data.List as List



-- | To get Ngrams out of a Gargantext.Viz.Phylo.PhyloGroup
getNgrams :: PhyloGroup -> [Int]
getNgrams =  _phylo_groupNgrams

getGroups :: Phylo -> [PhyloGroup]
getGroups = view (phylo_periods . traverse . phylo_periodLevels . traverse . phylo_levelGroups)

getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = view (phylo_groupId)

getGroupLvl :: PhyloGroup -> Int
getGroupLvl = snd . fst . getGroupId

getGroupPeriod :: PhyloGroup -> (Date,Date)
getGroupPeriod = fst . fst . getGroupId

getGroupsByLevelAndPeriod :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsByLevelAndPeriod lvl period p = List.filter testGroup  (getGroups p)
  where
    testGroup group = (getGroupLvl    group == lvl   )
                   && (getGroupPeriod group == period)


