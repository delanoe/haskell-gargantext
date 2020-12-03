{-|
Module      : Gargantext.Database.Lists
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Action.Metrics.Lists
  where

import Prelude hiding (null, id, map, sum)
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import qualified Gargantext.Database.Action.Metrics as Metrics
import Gargantext.API.Ngrams.Types (TabType(..))
import Gargantext.Core.Types -- (NodePoly(..), NodeCorpus, ListId)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Prelude hiding (sum, head)
import Gargantext.Core.Text.Metrics (Scored(..))

{-
trainModel :: FlowCmdM env ServantErr m
             => Username -> m Score
trainModel u = do
  rootId <- _node_id <$> getRoot u
  (id:ids)   <- getCorporaWithParentId rootId
  (s,_model) <- case length ids >0 of
        True  ->  grid 100 150 (getMetrics 
        False -> panic "Gargantext.Database.Lists.trainModel : not enough corpora"
  --}


getMetrics' :: FlowCmdM env err m
             => CorpusId -> Maybe ListId -> TabType -> Maybe Int
             -> m (Map.Map ListType [Vec.Vector Double])
getMetrics' cId maybeListId tabType maybeLimit = do
  (ngs', scores) <- Metrics.getMetrics cId maybeListId tabType maybeLimit

  let
    metrics      = map (\(Scored t s1 s2) -> (listType t ngs', [Vec.fromList [s1,s2]])) scores
    listType t m = maybe (panic errorMsg) fst $ Map.lookup t m
    errorMsg     = "API.Node.metrics: key absent"
  
  {-
  _ <- Learn.grid 100 110 metrics' metrics'
  --}
  pure $ Map.fromListWith (<>) $ Vec.toList metrics

