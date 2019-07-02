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
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.Phylo.API
  where

import Data.String.Conversions
--import Control.Monad.Reader (ask)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import Data.Text (Text)
import Data.Map  (empty)
import Data.Swagger
import Gargantext.API.Types
import Gargantext.Database.Types.Node (PhyloId, ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Main
import Gargantext.Viz.Phylo.Aggregates
import Gargantext.Viz.Phylo.Example
import Gargantext.Viz.Phylo.Tools
--import Gargantext.Viz.Phylo.View.ViewMaker
import Gargantext.Viz.Phylo.LevelMaker
import Servant
import Servant.Job.Utils (swaggerOptions)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (parseUrlPiece, readTextData)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Media ((//), (/:))

------------------------------------------------------------------------
type PhyloAPI = Summary "Phylo API"
              :> GetPhylo
        --    :<|> PutPhylo
            :<|> PostPhylo


phyloAPI :: PhyloId -> GargServer PhyloAPI
phyloAPI n = getPhylo'  n
        -- :<|> putPhylo  n
        :<|> postPhylo n

newtype SVG = SVG DB.ByteString

instance ToSchema SVG
  where
    declareNamedSchema = undefined {-genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}
    -}
    -- undefined
    --genericDeclareNamedSchemaUnrestricted (swaggerOptions "")

instance Show SVG where
  show (SVG a) = show a

instance Accept SVG where
   contentType _ = "SVG" // "image/svg+xml" /: ("charset", "utf-8")

instance Show a => MimeRender PlainText a where
   mimeRender _ val = cs ("" <> show val)

instance MimeRender SVG SVG where
   mimeRender _ (SVG s) = DBL.fromStrict s


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
              :> QueryParam "order"      Order
              :> QueryParam "export"    ExportMode
              :> QueryParam "display"    DisplayMode
              :> QueryParam "verbose"     Bool
              :> Get '[SVG] SVG

-- | TODO
-- Add real text processing
-- Fix Filter parameters
{-
getPhylo :: PhyloId -> GargServer GetPhylo
getPhylo _phyloId _lId l f b l' ms x y z ts s o e d b' = do
  let
    fs' = maybe (Just []) (\p -> Just [p]) $ LonelyBranch <$> (LBParams <$> x <*> y <*> z)
    so  = (,) <$> s <*> o
    q = initPhyloQueryView l f b l' ms fs' ts so e d b'
  -- | TODO remove phylo for real data here
  pure (toPhyloView  q phylo)
  -- TODO remove phylo for real data here
-}

getPhylo' :: PhyloId -> GargServer GetPhylo
getPhylo' _phyloId _lId _l _f _b _l' _ms _x _y _z _ts _s _o _e _d _b' = do
  p <- liftIO $ viewPhylo2Svg phyloView
  pure (SVG p)
------------------------------------------------------------------------
{-
type PutPhylo = (Put '[JSON] Phylo  )
--putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo :: PhyloId -> GargServer PutPhylo
putPhylo = undefined
-}
------------------------------------------------------------------------
type PostPhylo =  QueryParam "listId" ListId
               :> ReqBody '[JSON] PhyloQueryBuild
               :> (Post '[JSON] Phylo)

postPhylo :: CorpusId -> GargServer PostPhylo
postPhylo _n _lId q = do
  -- TODO get Reader settings
  -- s <- ask
  let
    vrs = Just ("1" :: Text)
    sft = Just (Software "Gargantext" "4")
    prm = initPhyloParam vrs sft (Just q)
  pure (toPhyloBase q prm (parseDocs (initFoundationsRoots actants) corpus) termList empty)


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
instance ToSchema PhyloFis
instance ToSchema PhyloBranch
instance ToSchema PhyloEdge
instance ToSchema PhyloGroup
instance ToSchema PhyloLevel
instance ToSchema PhyloNode
instance ToSchema PhyloParam
instance ToSchema PhyloFoundations
instance ToSchema PhyloPeriod
instance ToSchema PhyloQueryBuild
instance ToSchema PhyloView
instance ToSchema RCParams
instance ToSchema LBParams
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


instance ToParamSchema   ExportMode
instance FromHttpApiData ExportMode
  where
    parseUrlPiece = readTextData    


instance FromHttpApiData Sort
  where
    parseUrlPiece = readTextData
instance ToParamSchema Sort


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


