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

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Metrics
    where

import Data.Time (UTCTime)
import Servant
import qualified Data.Map as Map

import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.NTree
import Gargantext.Core.Types (CorpusId, Limit, ListId, ListType(..))
import qualified Gargantext.Database.Action.Metrics as Metrics
import Gargantext.Database.Action.Flow
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metric(..), Metrics(..))
import Gargantext.Database.Prelude
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
                :<|> Summary "SepGen IncExc metrics update"
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
getScatter cId maybeListId tabType maybeLimit = do
  (ngs', scores) <- Metrics.getMetrics cId maybeListId tabType maybeLimit

  let
    metrics      = map (\(Scored t s1 s2) -> Metric t (log' 5 s1) (log' 2 s2) (listType t ngs')) scores
    log' n x     = 1 + (if x <= 0 then 0 else (log $ (10^(n::Int)) * x))
    listType t m = maybe (panic errorMsg) fst $ Map.lookup t m
    errorMsg     = "API.Node.metrics: key absent"

  pure $ Metrics metrics


updateScatter :: FlowCmdM env err m =>
  CorpusId
  -> Maybe ListId
  -> TabType
  -> Maybe Limit
  -> m ()
updateScatter cId maybeListId tabType maybeLimit = do
  (_ngs', _scores) <- Metrics.getMetrics cId maybeListId tabType maybeLimit

  let
    -- metrics      = map (\(Scored t s1 s2) -> Metric t (log' 5 s1) (log' 2 s2) (listType t ngs')) scores
    -- log' n x     = 1 + (if x <= 0 then 0 else (log $ (10^(n::Int)) * x))
    -- listType t m = maybe (panic errorMsg) fst $ Map.lookup t m
    -- errorMsg     = "API.Node.metrics: key absent"

  --pure $ Metrics metrics
  pure ()



-- TODO add start / end
getChart :: CorpusId -> Maybe UTCTime -> Maybe UTCTime -> Cmd err (ChartMetrics Histo)
getChart cId _start _end = do
  h <- histoData cId
  pure (ChartMetrics h)

getPie :: FlowCmdM env err m => CorpusId -> Maybe UTCTime -> Maybe UTCTime -> TabType -> m (ChartMetrics Histo)
getPie cId _start _end tt = do
  p <- pieData cId (ngramsTypeFromTabType tt) GraphTerm
  pure (ChartMetrics p)

getTree :: FlowCmdM env err m => CorpusId -> Maybe UTCTime -> Maybe UTCTime -> TabType -> ListType -> m (ChartMetrics [MyTree])
getTree cId _start _end tt lt = do
  p <- treeData cId (ngramsTypeFromTabType tt) lt
  pure (ChartMetrics p)



