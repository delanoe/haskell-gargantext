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


module Gargantext.Viz.Phylo.API
  where

--{-
import Gargantext.Database.Types.Node (PhyloId, ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Example

getPhylo :: PhyloId -> Maybe PhyloQueryView -> PhyloView
getPhylo _phyloId _phyloQueryView = phyloView

postPhylo :: CorpusId -> Maybe ListId -> PhyloQueryBuild -> Phylo
postPhylo = undefined

putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo = undefined

deletePhylo :: PhyloId -> IO ()
deletePhylo = undefined

--}
