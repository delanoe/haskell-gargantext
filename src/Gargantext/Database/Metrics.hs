{-|
Module      : Gargantext.Database.Metrics
Description : Get Metrics from Storage (Database like)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Node API
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Database.Metrics
  where

import Data.Map (Map)
import Data.Text (Text)
import Gargantext.API.Ngrams (TabType(..), ngramsTypeFromTabType)
import Gargantext.API.Ngrams.Tools (filterListWithRoot, groupNodesByNgrams, Diagonal(..), getCoocByNgrams, mapTermListRoot, RootTerm)
import Gargantext.Core.Types (ListType(..), Limit)
import Gargantext.Database.Flow (FlowCmdM)
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser, getTficfWith)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Types.Node (ListId, CorpusId, HyperdataCorpus)
import Gargantext.Database.Flow (getOrMkRootWithCorpus)
import Gargantext.Database.Config (userMaster)
import Gargantext.Prelude
import Gargantext.Text.Metrics (scored, Scored(..), localMetrics, toScored)
import qualified Data.Map    as Map
import qualified Data.Vector.Storable as Vec


getMetrics' :: FlowCmdM env err m
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m (Map Text (ListType, Maybe Text), [Scored Text])
getMetrics' cId maybeListId tabType maybeLimit = do
  (ngs, _, myCooc) <- getNgramsCooc cId maybeListId tabType maybeLimit 
  pure (ngs, scored myCooc)


getMetrics :: FlowCmdM env err m
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m (Map Text (ListType, Maybe Text), [Scored Text])
getMetrics cId maybeListId tabType maybeLimit = do
  (ngs, ngs', metrics)    <- getLocalMetrics cId maybeListId tabType maybeLimit
  
  (_masterUserId, _masterRootId, masterCorpusId) <- getOrMkRootWithCorpus userMaster "" (Nothing :: Maybe HyperdataCorpus)
  
  metrics' <- getTficfWith cId masterCorpusId (ngramsTypeFromTabType tabType) ngs'

  pure (ngs , toScored [metrics, Map.fromList $ map (\(a,b) -> (a, Vec.fromList [fst b])) $ Map.toList metrics'])


getLocalMetrics  :: (FlowCmdM env err m)
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
          -> m ( Map Text (ListType, Maybe Text)
               , Map Text (Maybe RootTerm)
                 , Map Text (Vec.Vector Double)
                 )
getLocalMetrics cId maybeListId tabType maybeLimit = do
  (ngs, ngs', myCooc) <- getNgramsCooc cId maybeListId tabType maybeLimit 
  pure (ngs, ngs', localMetrics myCooc)


getNgramsCooc :: (FlowCmdM env err m)
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m ( Map Text (ListType, Maybe Text)
                 , Map Text (Maybe RootTerm)
                 , Map (Text, Text) Int
                 )
getNgramsCooc cId maybeListId tabType maybeLimit = do
  (ngs', ngs) <- getNgrams cId maybeListId tabType
  
  let
    take' Nothing xs  = xs
    take' (Just n) xs = take n xs
  
  myCooc <- Map.filter (>1) <$> getCoocByNgrams (Diagonal True)
                            <$> groupNodesByNgrams ngs
                            <$> getNodesByNgramsOnlyUser cId (ngramsTypeFromTabType tabType)
                                                             (take' maybeLimit $ Map.keys ngs)
  pure $ (ngs', ngs, myCooc)



getNgrams :: (FlowCmdM env err m)
            => CorpusId -> Maybe ListId -> TabType
            -> m (Map Text (ListType, Maybe Text), Map Text (Maybe RootTerm))
getNgrams cId maybeListId tabType = do
  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  lists    <- mapTermListRoot [lId] (ngramsTypeFromTabType tabType)
  let maybeSyn = Map.unions $ map (\t -> filterListWithRoot t lists)
                             [GraphTerm, StopTerm, CandidateTerm]
  pure (lists, maybeSyn)

