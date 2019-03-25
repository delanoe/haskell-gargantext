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


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}

module Gargantext.Database.Metrics
  where

import Data.Map (Map)
import Data.Text (Text)
import Gargantext.API.Ngrams (TabType(..), ngramsTypeFromTabType)
import Gargantext.API.Ngrams.Tools
import Gargantext.Core.Types (ListType(..), Limit)
import Gargantext.Database.Flow (FlowCmdM)
import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Types.Node (ListId, CorpusId)
import Gargantext.Prelude
import Gargantext.Text.Metrics
import Servant (ServantErr)
import qualified Data.Map as Map

getMetrics' :: FlowCmdM env ServantErr m
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m (Map Text (ListType, Maybe Text), [Scored Text])
getMetrics' cId maybeListId tabType maybeLimit = do

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  let ngramsType = ngramsTypeFromTabType tabType

  ngs'    <- mapTermListRoot [lId] ngramsType
  let ngs = Map.unions $ map (\t -> filterListWithRoot t ngs')
                             [GraphTerm, StopTerm, CandidateTerm]
  
  let
    take' Nothing xs  = xs
    take' (Just n) xs = take n xs
  
  myCooc <- Map.filter (>1) <$> getCoocByNgrams True
                            <$> groupNodesByNgrams ngs
                            <$> getNodesByNgramsOnlyUser cId ngramsType (take' maybeLimit $ Map.keys ngs)

  pure $ (ngs', scored myCooc)



