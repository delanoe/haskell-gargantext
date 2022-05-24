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

module Gargantext.Core.Viz.Phylo.API
  where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Either
import Data.Maybe (fromMaybe)
import Data.Swagger
import Gargantext.API.Prelude
import Gargantext.Core.Types (TODO(..))
import Gargantext.Core.Viz.LegacyPhylo
import Gargantext.Core.Viz.Phylo (defaultConfig)
import Gargantext.Core.Viz.Phylo.API.Tools
import Gargantext.Core.Viz.Phylo.Example (phyloExample)
import Gargantext.Core.Viz.Phylo.Legacy.LegacyMain
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (PhyloId, ListId, CorpusId, UserId, NodeId(..))
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, defaultList)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Prelude
import Network.HTTP.Media ((//), (/:))
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (readTextData)
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
--instance Show a => MimeRender PlainText a where mimeRender _ val = cs ("" <> show val)
instance Accept SVG where contentType _ = "SVG" // "image/svg+xml" /: ("charset", "utf-8")
instance MimeRender SVG SVG where mimeRender _ (SVG s) = DBL.fromStrict s
instance MimeUnrender SVG SVG where mimeUnrender _ lbs = Right $ SVG (DBL.toStrict lbs)
instance Show SVG where show (SVG a) = show a
instance ToSchema SVG where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

------------------------------------------------------------------------
instance ToSchema Value where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

------------------------------------------------------------------------

data PhyloData = PhyloData { pd_corpusId :: NodeId
                           , pd_listId   :: NodeId
                           , pd_data     :: Value
                           }
  deriving (Generic)

instance FromJSON PhyloData
instance ToJSON PhyloData
instance ToSchema PhyloData

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
              -- :> Get '[SVG] SVG
              :> Get '[JSON] PhyloData


-- | TODO
-- Add real text processing
-- Fix Filter parameters
-- TODO fix parameters to default config that should be in Node
getPhylo :: PhyloId -> GargServer GetPhylo
getPhylo phyloId lId _level _minSizeBranch = do
  corpusId <- fromMaybe (panic $ "[G.C.V.Phylo.API] no parent for NodeId " <> (cs $ show phyloId))
          <$> getClosestParentIdByType phyloId NodeCorpus
  listId   <- case lId of
                Nothing -> defaultList corpusId
                Just ld -> pure ld
  theData <- getPhyloDataJson phyloId
  -- printDebug "getPhylo" theData
  pure $ PhyloData corpusId listId theData



getPhyloDataJson :: PhyloId -> GargNoServer Value
getPhyloDataJson phyloId = do
  phyloData <- fromMaybe phyloExample <$> getPhyloData phyloId
  phyloJson <- liftBase $ phylo2dot2json phyloData
  pure phyloJson


-- getPhyloDataSVG phId _lId l msb  = do
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

postPhylo :: PhyloId -> UserId -> GargServer PostPhylo
postPhylo phyloId _userId _lId = do
  -- TODO get Reader settings
  -- s <- ask
  -- let
    -- _vrs = Just ("1" :: Text)
    -- _sft = Just (Software "Gargantext" "4")
    -- _prm = initPhyloParam vrs sft (Just q)
  corpusId <- getClosestParentIdByType phyloId NodeCorpus
  phy <- flowPhyloAPI defaultConfig (fromMaybe (panic "[G.C.V.P.API] no corpus ID found") corpusId) -- params
  -- phyloId <- insertNodes [node NodePhylo "Phylo" (HyperdataPhylo Nothing (Just phy)) (Just corpusId) userId]
  _ <- updateHyperdata phyloId (HyperdataPhylo Nothing (Just phy))
  pure phyloId

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

