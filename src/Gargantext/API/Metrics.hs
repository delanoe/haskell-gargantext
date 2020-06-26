{-|
Module      : Gargantext.API.Metrics
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Metrics API

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Metrics
    where

import Control.Lens
import Data.Time (UTCTime)
import Servant
import qualified Data.Map as Map

import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.NTree
import Gargantext.Core.Types (CorpusId, Limit, ListId, ListType(..))
import qualified Gargantext.Database.Action.Metrics as Metrics
import Gargantext.Database.Action.Flow
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataList(..))
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metric(..), Metrics(..))
import Gargantext.Database.Query.Table.Node (defaultList, getNodeWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Text.Metrics (Scored(..))
import Gargantext.Viz.Chart
import Gargantext.Viz.Types

-------------------------------------------------------------
-- | Scatter metrics API
type ScatterAPI = Summary "SepGen IncExc metrics"
                  :> QueryParam  "list"       ListId
                  :> QueryParamR "ngramsType" TabType
                  :> QueryParam  "limit"      Int
                  :> Get '[JSON] Metrics
              :<|> Summary "Scatter update"
                  :> QueryParam  "list"       ListId
                  :> QueryParamR "ngramsType" TabType
                  :> QueryParam  "limit"      Int
                  :> Post '[JSON] ()

getScatter :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m Metrics
getScatter cId maybeListId tabType _maybeLimit = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_scatter = mChart }) = node ^. node_hyperdata

  case mChart of
    Just chart -> pure chart
    Nothing    -> do
      s <- updateScatter' cId maybeListId tabType Nothing
      pure s

updateScatter :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m ()
updateScatter cId maybeListId tabType maybeLimit = do
  _ <- updateScatter' cId maybeListId tabType maybeLimit
  pure ()

updateScatter' :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m Metrics
updateScatter' cId maybeListId tabType maybeLimit = do
  (ngs', scores) <- Metrics.getMetrics cId maybeListId tabType maybeLimit

  let
    metrics      = map (\(Scored t s1 s2) -> Metric t (log' 5 s1) (log' 2 s2) (listType t ngs')) scores
    log' n x     = 1 + (if x <= 0 then 0 else (log $ (10^(n::Int)) * x))
    listType t m = maybe (panic errorMsg) fst $ Map.lookup t m
    errorMsg     = "API.Node.metrics: key absent"

  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_chart = hdc
                     , hd_list = hdl
                     , hd_pie = hdp
                     , hd_tree = hdt }) = node ^. node_hyperdata
  _ <- updateHyperdata listId $ HyperdataList hdc hdl hdp (Just $ Metrics metrics) hdt

  pure $ Metrics metrics


-------------------------------------------------------------
-- | Chart metrics API
type ChartApi = Summary " Chart API"
              :> QueryParam "from" UTCTime
              :> QueryParam "to"   UTCTime
              :> QueryParam  "list"       ListId
              :> QueryParamR "ngramsType" TabType
              :> Get '[JSON] (ChartMetrics Histo)
        :<|> Summary "Chart update"
                :> QueryParam  "list"       ListId
                :> QueryParamR "ngramsType" TabType
                :> QueryParam  "limit"      Int
                :> Post '[JSON] ()
               
-- TODO add start / end
getChart :: HasNodeError err
         => CorpusId
         -> Maybe UTCTime
         -> Maybe UTCTime
         -> Maybe ListId
         -> TabType
         -> Cmd err (ChartMetrics Histo)
getChart cId _start _end maybeListId tabType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_chart = mChart }) = node ^. node_hyperdata

  case mChart of
    Just chart -> pure chart
    Nothing    -> do
      h <- updateChart' cId maybeListId tabType Nothing
      pure h

updateChart :: HasNodeError err =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> Cmd err ()
updateChart cId maybeListId tabType maybeLimit = do
  _ <- updateChart' cId maybeListId tabType maybeLimit
  pure ()

updateChart' :: HasNodeError err =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> Cmd err (ChartMetrics Histo)
updateChart' cId maybeListId _tabType _maybeLimit = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_list = hdl
                     , hd_pie = hdp
                     , hd_scatter = hds
                     , hd_tree = hdt }) = node ^. node_hyperdata
  h <- histoData cId
  _ <- updateHyperdata listId $ HyperdataList (Just $ ChartMetrics h) hdl hdp hds hdt

  pure $ ChartMetrics h
-------------------------------------------------------------
-- | Pie metrics API
type PieApi = Summary "Pie Chart"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParam  "list"       ListId
           :> QueryParamR "ngramsType" TabType
           :> Get '[JSON] (ChartMetrics Histo)
        :<|> Summary "Pie Chart update"
                :> QueryParam  "list"       ListId
                :> QueryParamR "ngramsType" TabType
                :> QueryParam  "limit"      Int
                :> Post '[JSON] ()

getPie :: FlowCmdM env err m
       => CorpusId
       -> Maybe UTCTime
       -> Maybe UTCTime
       -> Maybe ListId
       -> TabType
       -> m (ChartMetrics Histo)
getPie cId _start _end maybeListId tabType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_pie = mChart }) = node ^. node_hyperdata

  case mChart of
    Just chart -> pure chart
    Nothing    -> do
      p <- updatePie' cId maybeListId tabType Nothing
      pure p

updatePie :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m ()
updatePie cId maybeListId tabType maybeLimit = do
  _ <- updatePie' cId maybeListId tabType maybeLimit
  pure ()

updatePie' :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m (ChartMetrics Histo)
updatePie' cId maybeListId tabType _maybeLimit = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_chart = hdc
                     , hd_list = hdl
                     , hd_scatter = hds
                     , hd_tree = hdt }) = node ^. node_hyperdata

  p <- pieData cId (ngramsTypeFromTabType tabType) MapTerm
  _ <- updateHyperdata listId $ HyperdataList hdc hdl (Just $ ChartMetrics p) hds hdt

  pure $ ChartMetrics p

-------------------------------------------------------------
-- | Tree metrics API

type TreeApi = Summary " Tree API"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParam  "list"       ListId
           :> QueryParamR "ngramsType" TabType
           :> QueryParamR "listType"   ListType
           :> Get '[JSON] (ChartMetrics [MyTree])
        :<|> Summary "Tree Chart update"
                :> QueryParam  "list"       ListId
                :> QueryParamR "ngramsType" TabType
                :> QueryParamR "listType"   ListType
                :> Post '[JSON] ()

                -- Depending on the Type of the Node, we could post
                -- New documents for a corpus
                -- New map list terms
             -- :<|> "process"  :> MultipartForm MultipartData :> Post '[JSON] Text

getTree :: FlowCmdM env err m
        => CorpusId
        -> Maybe UTCTime
        -> Maybe UTCTime
        -> Maybe ListId
        -> TabType
        -> ListType
        -> m (ChartMetrics [MyTree])
getTree cId _start _end maybeListId tabType listType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId

  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_tree = mChart }) = node ^. node_hyperdata
  case mChart of
    Just chart -> pure chart
    Nothing    -> do
      t <- updateTree' cId maybeListId tabType listType
      pure t

updateTree :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> ListType
  -> m ()
updateTree cId maybeListId tabType listType = do
  _ <- updateTree' cId maybeListId tabType listType
  pure ()

updateTree' :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> ListType
  -> m (ChartMetrics [MyTree])
updateTree' cId maybeListId tabType listType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId

  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let (HyperdataList { hd_chart = hdc
                     , hd_list = hdl
                     , hd_scatter = hds
                     , hd_pie = hdp }) = node ^. node_hyperdata
  t <- treeData cId (ngramsTypeFromTabType tabType) listType
  _ <- updateHyperdata listId $ HyperdataList hdc hdl hdp hds (Just $ ChartMetrics t)

  pure $ ChartMetrics t
