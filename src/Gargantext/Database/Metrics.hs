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


{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE FlexibleContexts  #-}


module Gargantext.Database.Metrics
  where

import Data.Map (Map)
import Data.Text (Text)
import Gargantext.API.Ngrams (TabType(..), ngramsTypeFromTabType)
import Gargantext.API.Ngrams.Tools
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Flow (FlowCmdM)
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Types.Node (ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Text.Metrics
import Servant (ServantErr)
import qualified Data.Map as Map

getMetrics' :: FlowCmdM env ServantErr m
            => CorpusId -> Maybe ListId -> Maybe TabType
            -> m (Map Text (ListType, Maybe Text), [Scored Text])
getMetrics' cId maybeListId maybeTabType = do

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  let ngramsType = ngramsTypeFromTabType maybeTabType

  ngs'    <- mapTermListRoot [lId] ngramsType
  let ngs = Map.unions $ map (\t -> filterListWithRoot t ngs')
                             [GraphTerm, StopTerm, CandidateTerm]

  myCooc <- Map.filter (>1) <$> getCoocByNgrams True
                            <$> groupNodesByNgrams ngs
                            <$> getNodesByNgramsOnlyUser cId ngramsType (Map.keys ngs)

  pure $ (ngs', scored myCooc)



