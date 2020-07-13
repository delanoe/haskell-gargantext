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

{-# LANGUAGE OverloadedLists    #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators      #-}

module Gargantext.Viz.Phylo.API
  where

import Control.Lens ((^?), _Just)
import Data.Proxy (Proxy(..))
import Data.String.Conversions
import Data.Swagger
import Gargantext.API.Prelude
import Gargantext.Core.Types (TODO(..))
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (PhyloId, ListId, CorpusId, UserId, NodeId(..))
import Gargantext.Database.Query.Table.Node (insertNodes, node, getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Example
import Gargantext.Viz.Phylo.Main
import Network.HTTP.Media ((//), (/:))
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (parseUrlPiece, readTextData)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL

------------------------------------------------------------------------
type PhyloAPI = Summary "Phylo API"
              :> GetPhylo
        --    :<|> PutPhylo
            :<|> PostPhylo


phyloAPI :: PhyloId -> UserId -> GargServer PhyloAPI
phyloAPI n u = getPhylo  n
        :<|> postPhylo n u
        -- :<|> putPhylo  n
        -- :<|> deletePhylo  n

newtype SVG = SVG DB.ByteString

instance ToSchema SVG
  where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

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
              :> QueryParam "minSizeBranch" MinSizeBranch
   {-           :> QueryParam "filiation"   Filiation
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
    -}
              :> Get '[SVG] SVG

-- | TODO
-- Add real text processing
-- Fix Filter parameters
getPhylo :: PhyloId -> GargServer GetPhylo
getPhylo phId _lId l msb  = do
  phNode     <- getNodeWith phId (Proxy :: Proxy HyperData)
  let
    level = maybe 2 identity l
    branc = maybe 2 identity msb
    maybePhylo = phNode ^? ( node_hyperdata . hd_data . _Just)

  p <- liftBase $ viewPhylo2Svg
                $ viewPhylo level branc
                $ maybe phyloFromQuery identity maybePhylo
  pure (SVG p)
------------------------------------------------------------------------
type PostPhylo =  QueryParam "listId" ListId
               -- :> ReqBody '[JSON] PhyloQueryBuild
               :> (Post '[JSON] NodeId)

postPhylo :: CorpusId -> UserId -> GargServer PostPhylo
postPhylo n userId _lId = do
  -- TODO get Reader settings
  -- s <- ask
  let
    -- _vrs = Just ("1" :: Text)
    -- _sft = Just (Software "Gargantext" "4")
    -- _prm = initPhyloParam vrs sft (Just q)
  phy  <- flowPhylo n
  pId <- insertNodes [node NodePhylo "Phylo" (HyperdataPhylo Nothing (Just phy)) (Just n) userId]
  pure $ NodeId (fromIntegral pId)

------------------------------------------------------------------------
-- | DELETE Phylo == delete a node
------------------------------------------------------------------------
------------------------------------------------------------------------
{-
type PutPhylo = (Put '[JSON] Phylo  )
--putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo :: PhyloId -> GargServer PutPhylo
putPhylo = undefined
-}


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

instance ToSchema Order

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


