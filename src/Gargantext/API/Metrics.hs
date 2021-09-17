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

{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Metrics
    where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Gargantext.API.HashedResponse
import Gargantext.API.Ngrams.NgramsTree
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Text.Metrics (Scored(..), {-normalizeGlobal,-} normalizeLocal)
import Gargantext.Core.Types (CorpusId, Limit, ListId, ListType(..))
import Gargantext.Core.Viz.Chart
import Gargantext.Core.Viz.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataList(..), hl_chart, hl_pie, hl_scatter, hl_tree)
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metric(..), Metrics(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node (defaultList, getNodeWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Servant
import qualified Data.HashMap.Strict                as HashMap
import qualified Gargantext.Database.Action.Metrics as Metrics

-------------------------------------------------------------
-- | Scatter metrics API
type ScatterAPI = Summary "SepGen IncExc metrics"
                  :> QueryParam  "list"       ListId
                  :> QueryParamR "ngramsType" TabType
                  :> QueryParam  "limit"      Int
                  :> Get '[JSON] (HashedResponse Metrics)
              :<|> Summary "Scatter update"
                  :> QueryParam  "list"       ListId
                  :> QueryParamR "ngramsType" TabType
                  :> QueryParam  "limit"      Int
                  :> Post '[JSON] ()
              :<|> "hash" :> Summary "Scatter Hash"
                          :> QueryParam  "list"       ListId
                          :> QueryParamR "ngramsType" TabType
                          :> Get '[JSON] Text

scatterApi :: NodeId -> GargServer ScatterAPI
scatterApi id' = getScatter id'
            :<|> updateScatter id'
            :<|> getScatterHash id'

getScatter :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m (HashedResponse Metrics)
getScatter cId maybeListId tabType _maybeLimit = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let HyperdataList { _hl_scatter = scatterMap } = node ^. node_hyperdata
      mChart = HashMap.lookup tabType scatterMap

  chart <- case mChart of
    Just chart -> pure chart
    Nothing    -> do
      updateScatter' cId maybeListId tabType Nothing

  pure $ constructHashedResponse chart

updateScatter :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m ()
updateScatter cId maybeListId tabType maybeLimit = do
  printDebug "[updateScatter] cId" cId
  printDebug "[updateScatter] maybeListId" maybeListId
  printDebug "[updateScatter] tabType" tabType
  printDebug "[updateScatter] maybeLimit" maybeLimit
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
    metrics      = fmap (\(Scored t s1 s2) -> Metric { m_label = unNgramsTerm t
                                                     , m_x = s1
                                                     , m_y = s2
                                                     , m_cat = listType t ngs' })
                 $ fmap normalizeLocal scores
    listType t m = maybe (panic errorMsg) fst $ HashMap.lookup t m
    errorMsg     = "API.Node.metrics: key absent"

  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let hl = node ^. node_hyperdata
      scatterMap = hl ^. hl_scatter
  _ <- updateHyperdata listId $ hl { _hl_scatter = HashMap.insert tabType (Metrics metrics) scatterMap }

  pure $ Metrics metrics

getScatterHash :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> m Text
getScatterHash cId maybeListId tabType = do
  hash <$> getScatter cId maybeListId tabType Nothing


-------------------------------------------------------------
-- | Chart metrics API
type ChartApi = Summary " Chart API"
              :> QueryParam "from" UTCTime
              :> QueryParam "to"   UTCTime
              :> QueryParam  "list"       ListId
              :> QueryParamR "ngramsType" TabType
              :> Get '[JSON] (HashedResponse (ChartMetrics Histo))
            :<|> Summary "Chart update"
               :> QueryParam  "list"       ListId
               :> QueryParamR "ngramsType" TabType
               :> QueryParam  "limit"      Int
               :> Post '[JSON] ()
            :<|> "hash" :> Summary "Chart Hash"
               :> QueryParam  "list"       ListId
               :> QueryParamR "ngramsType" TabType
               :> Get '[JSON] Text

chartApi :: NodeId -> GargServer ChartApi
chartApi id' = getChart id'
          :<|> updateChart id'
          :<|> getChartHash id'

-- TODO add start / end
getChart :: FlowCmdM env err m =>
            CorpusId
         -> Maybe UTCTime
         -> Maybe UTCTime
         -> Maybe ListId
         -> TabType
         -> m (HashedResponse (ChartMetrics Histo))
getChart cId _start _end maybeListId tabType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let chartMap = node ^. node_hyperdata ^. hl_chart
      mChart = HashMap.lookup tabType chartMap

  chart <- case mChart of
    Just chart -> pure chart
    Nothing    -> do
      updateChart' cId maybeListId tabType Nothing

  pure $ constructHashedResponse chart

updateChart :: HasNodeError err =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> Cmd err ()
updateChart cId maybeListId tabType maybeLimit = do
  printDebug "[updateChart] cId" cId
  printDebug "[updateChart] maybeListId" maybeListId
  printDebug "[updateChart] tabType" tabType
  printDebug "[updateChart] maybeLimit" maybeLimit
  _ <- updateChart' cId maybeListId tabType maybeLimit
  pure ()

updateChart' :: HasNodeError err =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> Cmd err (ChartMetrics Histo)
updateChart' cId maybeListId tabType _maybeLimit = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let hl = node ^. node_hyperdata
      chartMap = hl ^. hl_chart
  h <- histoData cId
  _ <- updateHyperdata listId $ hl { _hl_chart = HashMap.insert tabType (ChartMetrics h) chartMap }

  pure $ ChartMetrics h


getChartHash :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> m Text
getChartHash cId maybeListId tabType = do
  hash <$> getChart cId Nothing Nothing maybeListId tabType
 
-------------------------------------------------------------
-- | Pie metrics API
type PieApi = Summary "Pie Chart"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParam  "list"       ListId
           :> QueryParamR "ngramsType" TabType
           :> Get '[JSON] (HashedResponse (ChartMetrics Histo))
         :<|> Summary "Pie Chart update"
             :> QueryParam  "list"       ListId
             :> QueryParamR "ngramsType" TabType
             :> QueryParam  "limit"      Int
             :> Post '[JSON] ()
         :<|> "hash" :> Summary "Pie Hash"
                     :> QueryParam  "list"       ListId
                     :> QueryParamR "ngramsType" TabType
                     :> Get '[JSON] Text

pieApi :: NodeId -> GargServer PieApi
pieApi id' = getPie id'
        :<|> updatePie id'
        :<|> getPieHash id'

getPie :: FlowCmdM env err m
       => CorpusId
       -> Maybe UTCTime
       -> Maybe UTCTime
       -> Maybe ListId
       -> TabType
       -> m (HashedResponse (ChartMetrics Histo))
getPie cId _start _end maybeListId tabType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId
  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let pieMap = node ^. node_hyperdata ^. hl_pie
      mChart = HashMap.lookup tabType pieMap

  chart <- case mChart of
    Just chart -> pure chart
    Nothing    -> do
      updatePie' cId maybeListId tabType Nothing

  pure $ constructHashedResponse chart

updatePie :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m ()
updatePie cId maybeListId tabType maybeLimit = do
  printDebug "[updatePie] cId" cId
  printDebug "[updatePie] maybeListId" maybeListId
  printDebug "[updatePie] tabType" tabType
  printDebug "[updatePie] maybeLimit" maybeLimit
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
  let hl = node ^. node_hyperdata
      pieMap = hl ^. hl_pie

  p <- chartData cId (ngramsTypeFromTabType tabType) MapTerm
  _ <- updateHyperdata listId $ hl { _hl_pie = HashMap.insert tabType (ChartMetrics p) pieMap }

  pure $ ChartMetrics p

getPieHash :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> m Text
getPieHash cId maybeListId tabType = do
  hash <$> getPie cId Nothing Nothing maybeListId tabType

-------------------------------------------------------------
-- | Tree metrics API

type TreeApi = Summary " Tree API"
           :> QueryParam "from" UTCTime
           :> QueryParam "to"   UTCTime
           :> QueryParam  "list"       ListId
           :> QueryParamR "ngramsType" TabType
           :> QueryParamR "listType"   ListType
           :> Get '[JSON] (HashedResponse (ChartMetrics (Vector NgramsTree)))
        :<|> Summary "Tree Chart update"
                :> QueryParam  "list"       ListId
                :> QueryParamR "ngramsType" TabType
                :> QueryParamR "listType"   ListType
                :> Post '[JSON] ()
          :<|> "hash" :>
                 Summary "Tree Hash"
              :> QueryParam  "list"       ListId
              :> QueryParamR "ngramsType" TabType
              :> QueryParamR "listType"   ListType
              :> Get '[JSON] Text
treeApi :: NodeId -> GargServer TreeApi
treeApi id' = getTree id'
         :<|> updateTree id'
         :<|> getTreeHash id'

getTree :: FlowCmdM env err m
        => CorpusId
        -> Maybe UTCTime
        -> Maybe UTCTime
        -> Maybe ListId
        -> TabType
        -> ListType
        -> m (HashedResponse (ChartMetrics (Vector NgramsTree)))
getTree cId _start _end maybeListId tabType listType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId

  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let treeMap = node ^. node_hyperdata ^. hl_tree
      mChart = HashMap.lookup tabType treeMap

  chart <- case mChart of
    Just chart -> pure chart
    Nothing    -> do
      updateTree' cId maybeListId tabType listType

  pure $ constructHashedResponse chart

updateTree :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> ListType
  -> m ()
updateTree cId maybeListId tabType listType = do
  printDebug "[updateTree] cId" cId
  printDebug "[updateTree] maybeListId" maybeListId
  printDebug "[updateTree] tabType" tabType
  printDebug "[updateTree] listType" listType
  _ <- updateTree' cId maybeListId tabType listType
  pure ()

updateTree' :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> ListType
  -> m (ChartMetrics (Vector NgramsTree))
updateTree' cId maybeListId tabType listType = do
  listId <- case maybeListId of
    Just lid -> pure lid
    Nothing  -> defaultList cId

  node <- getNodeWith listId (Proxy :: Proxy HyperdataList)
  let hl      = node ^. node_hyperdata
      treeMap = hl  ^. hl_tree
  t <- treeData cId (ngramsTypeFromTabType tabType) listType
  _ <- updateHyperdata listId $ hl { _hl_tree = HashMap.insert tabType (ChartMetrics t) treeMap }

  pure $ ChartMetrics t

getTreeHash :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> ListType
  -> m Text
getTreeHash cId maybeListId tabType listType = do
  hash <$> getTree cId Nothing Nothing maybeListId tabType listType
