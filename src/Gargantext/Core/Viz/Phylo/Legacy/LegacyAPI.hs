{-|
Module      : Gargantext.Core.Viz.Phylo.API
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

module Gargantext.Core.Viz.Phylo.Legacy.LegacyAPI
  where

-- import Data.Maybe (fromMaybe)
-- import Control.Lens ((^.))
import Data.String.Conversions
--import Control.Monad.Reader (ask)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import Data.Swagger
import Network.HTTP.Media ((//), (/:))
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (readTextData)

import Gargantext.API.Prelude
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (PhyloId, ListId, CorpusId, UserId, NodeId(..))
import Gargantext.Database.Query.Table.Node (insertNodes, node)
-- import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Core.Viz.LegacyPhylo
import Gargantext.Core.Viz.Phylo.Legacy.LegacyMain
-- import Gargantext.Core.Viz.Phylo.Example
import Gargantext.Core.Types (TODO(..))

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
getPhylo _ _lId _ _  = undefined 
-- getPhylo phId _lId l msb  = do 
  -- phNode     <- getNodeWith phId (Proxy :: Proxy HyperdataPhylo)
  -- let
  --   level = fromMaybe 2 l
  --   branc = fromMaybe 2 msb
  --   maybePhylo = phNode ^. (node_hyperdata . hp_data)

  -- p <- liftBase $ viewPhylo2Svg
  --               $ viewPhylo level branc
  --               $ fromMaybe phyloFromQuery maybePhylo
  -- pure (SVG p)


------------------------------------------------------------------------
type PostPhylo =  QueryParam "listId" ListId
               -- :> ReqBody '[JSON] PhyloQueryBuild
               :> (Post '[JSON] NodeId)

postPhylo :: CorpusId -> UserId -> GargServer PostPhylo
postPhylo corpusId userId _lId = do
  -- TODO get Reader settings
  -- s <- ask
  -- let
    -- _vrs = Just ("1" :: Text)
    -- _sft = Just (Software "Gargantext" "4")
    -- _prm = initPhyloParam vrs sft (Just q)
  phy <- flowPhylo corpusId -- params
  phyloId <- insertNodes [node NodePhylo "Phylo" (HyperdataPhylo Nothing (Just phy)) (Just corpusId) userId]
  pure $ NodeId (fromIntegral phyloId)

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
-- instance Arbitrary Phylo             where arbitrary     = elements [phylo]
instance Arbitrary PhyloGroup        where arbitrary     = elements []
-- instance Arbitrary PhyloView         where arbitrary     = elements [phyloView]
instance FromHttpApiData DisplayMode where parseUrlPiece = readTextData
instance FromHttpApiData ExportMode  where parseUrlPiece = readTextData
instance FromHttpApiData Filiation   where parseUrlPiece = readTextData
instance FromHttpApiData Metric      where parseUrlPiece = readTextData
instance FromHttpApiData Order       where parseUrlPiece = readTextData
instance FromHttpApiData Sort        where parseUrlPiece = readTextData
instance FromHttpApiData Tagger      where parseUrlPiece = readTextData
instance FromHttpApiData [Metric]    where parseUrlPiece = readTextData
instance FromHttpApiData [Tagger]    where parseUrlPiece = readTextData
instance ToParamSchema   DisplayMode
instance ToParamSchema   ExportMode
instance ToParamSchema   Filiation
instance ToParamSchema   Tagger
instance ToParamSchema Metric
instance ToParamSchema Order
instance ToParamSchema Sort
instance ToSchema Order


