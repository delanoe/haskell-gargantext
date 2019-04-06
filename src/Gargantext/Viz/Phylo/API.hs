{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}   -- allows to write Text literals
{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists

module Gargantext.Viz.Phylo.API
  where

import Data.Swagger
import Servant.Job.Utils (swaggerOptions)
import Gargantext.Database.Types.Node (PhyloId, ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Example
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


getPhylo :: PhyloId -> PhyloView
getPhylo _phyloId = phyloView
--getPhylo :: PhyloId -> Maybe PhyloQueryView -> PhyloView
--getPhylo _phyloId _phyloQueryView = phyloView

postPhylo :: CorpusId -> Maybe ListId -> PhyloQueryBuild -> Phylo
postPhylo = undefined

putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo = undefined

deletePhylo :: PhyloId -> IO ()
deletePhylo = undefined


-- | Instances

instance ToSchema Cluster
instance ToSchema EdgeType
instance ToSchema Filiation
instance ToSchema Filter
instance ToSchema FisParams
instance ToSchema HammingParams
instance ToSchema LouvainParams
instance ToSchema Metric
instance ToSchema PhyloBranch
instance ToSchema PhyloEdge
instance ToSchema PhyloNode
instance ToSchema PhyloParam
instance ToSchema PhyloQueryBuild
instance ToSchema PhyloView
instance ToSchema RCParams
instance ToSchema SBParams
instance ToSchema Software
instance ToSchema WLJParams

instance ToSchema Proximity
  where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
                       $ swaggerOptions ""

instance Arbitrary PhyloView
  where
    arbitrary = elements [phyloView]

