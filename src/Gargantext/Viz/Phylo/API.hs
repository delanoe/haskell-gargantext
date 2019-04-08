{-|
Module      : Gargantext.Viz.Phylo.API
Description : Phylo API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}   -- allows to write Text literals
{-# LANGUAGE OverloadedLists    #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Gargantext.Viz.Phylo.API
  where

import Data.Swagger
import Gargantext.API.Types
import Gargantext.Database.Types.Node (PhyloId, ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Example
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.View.ViewMaker
import Servant
import Servant.Job.Utils (swaggerOptions)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (parseUrlPiece, readTextData)

------------------------------------------------------------------------
type PhyloAPI = Summary "Phylo API"
     --         :> QueryParam "param" PhyloQueryView
             -- :<|> 
              :> GetPhylo
            :<|> PutPhylo
              -- :<|> Capture "id" PhyloId :> Post '[JSON] Phylo
              -- :<|> Capture "id" PhyloId :> Put  '[JSON] Phylo


phyloAPI :: PhyloId -> GargServer PhyloAPI
phyloAPI n = getPhylo n
        :<|> putPhylo n
           -- :<|> pure . (postPhylo n)


------------------------------------------------------------------------
type GetPhylo =  QueryParam "listId"      ListId
              :> QueryParam "level"       Level
              :> QueryParam "filiation"   Filiation
              :> QueryParam "childs"      Bool
              :> QueryParam "depth"       Level
              :> QueryParam "metrics"    [Metric]
              :> QueryParam "periodsInf" Int
              :> QueryParam "periodsSup" Int
              :> QueryParam "minNodes"   Int
              :> QueryParam "taggers"    [Tagger]
              :> QueryParam "sort"       Sort
              :> QueryParam "sort"       Order
              :> QueryParam "display"    DisplayMode
              :> QueryParam "verbose"     Bool
              :> Get '[JSON] PhyloView

-- | TODO
-- Add real text processing
-- Fix Filter parameters
getPhylo :: PhyloId -> GargServer GetPhylo
getPhylo _phyloId _lId l f b l' ms x y z ts s o d b' = do
  let
    fs' = maybe (Just []) (\p -> Just [p]) $ SmallBranch <$> (SBParams <$> x <*> y <*> z)
    so  = (,) <$> s <*> o
    q = initPhyloQueryView l f b l' ms fs' ts so d b'
  -- | TODO remove phylo for real data here
  pure (toPhyloView  q phylo)

------------------------------------------------------------------------
type PutPhylo = (Put '[JSON] Phylo  )
--putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo :: PhyloId -> GargServer PutPhylo
putPhylo = undefined


------------------------------------------------------------------------
type PostPhylo = (Post '[JSON] Phylo)
--postPhylo :: CorpusId -> Maybe ListId -> PhyloQueryBuild -> Phylo
postPhylo :: CorpusId -> Phylo
postPhylo = undefined

------------------------------------------------------------------------
-- | DELETE Phylo == delete a node
------------------------------------------------------------------------

-- | Instances
instance Arbitrary PhyloView
  where
    arbitrary = elements [phyloView]

-- | TODO add phyloGroup ex
instance Arbitrary PhyloGroup
  where
    arbitrary = elements []

instance Arbitrary Phylo
  where
    arbitrary = elements [phylo]




instance ToSchema Cluster
instance ToSchema EdgeType
instance ToSchema Filiation
instance ToSchema Filter
instance ToSchema FisParams
instance ToSchema HammingParams
instance ToSchema LouvainParams
instance ToSchema Metric
instance ToSchema Order
instance ToSchema Phylo
instance ToSchema PhyloBranch
instance ToSchema PhyloEdge
instance ToSchema PhyloGroup
instance ToSchema PhyloLevel
instance ToSchema PhyloNode
instance ToSchema PhyloParam
instance ToSchema PhyloPeaks
instance ToSchema PhyloPeriod
instance ToSchema PhyloQueryBuild
instance ToSchema PhyloView
instance ToSchema RCParams
instance ToSchema SBParams
instance ToSchema Software
instance ToSchema WLJParams


instance ToParamSchema Order
instance FromHttpApiData Order
  where
    parseUrlPiece = readTextData


instance ToParamSchema Metric
instance FromHttpApiData [Metric]
  where
    parseUrlPiece = readTextData
instance FromHttpApiData Metric
  where
    parseUrlPiece = readTextData


instance ToParamSchema   DisplayMode
instance FromHttpApiData DisplayMode
  where
    parseUrlPiece = readTextData


instance FromHttpApiData Sort
  where
    parseUrlPiece = readTextData
instance ToParamSchema Sort

instance (ToSchema a) => ToSchema (Tree a)
  where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
                       $ swaggerOptions ""

instance ToSchema Proximity
  where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
                       $ swaggerOptions ""


instance FromHttpApiData [Tagger]
  where
    parseUrlPiece = readTextData
instance FromHttpApiData Tagger
  where
    parseUrlPiece = readTextData
instance ToParamSchema   Tagger

instance FromHttpApiData Filiation
  where
    parseUrlPiece = readTextData
instance ToParamSchema   Filiation



