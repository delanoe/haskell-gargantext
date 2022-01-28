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

module Gargantext.Database.Action.Metrics
  where

import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import Gargantext.API.Ngrams.Tools (filterListWithRoot, groupNodesByNgrams, Diagonal(..), getCoocByNgrams, mapTermListRoot, RootTerm, getRepo')
import Gargantext.API.Ngrams.Types (TabType(..), ngramsTypeFromTabType, NgramsTerm)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.NodeStory
import Gargantext.Core.Text.Metrics (scored, Scored(..), {-localMetrics, toScored-})
import Gargantext.Core.Types (ListType(..), Limit, NodeType(..), ContextId)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsOnlyUser{-, getTficfWith-})
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Node (ListId, CorpusId)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.List           as List

getMetrics :: FlowCmdM env err m
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m (HashMap NgramsTerm (ListType, Maybe NgramsTerm), Vector (Scored NgramsTerm))
getMetrics cId maybeListId tabType maybeLimit = do
  (ngs, _, myCooc) <- getNgramsCooc cId maybeListId tabType maybeLimit
  -- TODO HashMap
  pure (ngs, scored myCooc)


getNgramsCooc :: (FlowCmdM env err m)
            => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
            -> m ( HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                 , HashMap NgramsTerm (Maybe RootTerm)
                 , HashMap (NgramsTerm, NgramsTerm) Int
                 )
getNgramsCooc cId maybeListId tabType maybeLimit = do
  (ngs', ngs) <- getNgrams cId maybeListId tabType

  lId  <- defaultList cId
  lIds <- selectNodesWithUsername NodeList userMaster

  myCooc <- HM.filter (>1) <$> getCoocByNgrams (Diagonal True)
                           <$> groupNodesByNgrams ngs
                           <$> getContextsByNgramsOnlyUser cId
                                                           (lIds <> [lId])
                                                           (ngramsTypeFromTabType tabType)
                                                           (take' maybeLimit $ HM.keys ngs)
  pure $ (ngs', ngs, myCooc)

-- Used for scores in Ngrams Table
getNgramsOccurrences :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
             -> m (HashMap NgramsTerm Int)
getNgramsOccurrences c l t ml = HM.map Set.size <$> getNgramsContexts c l t ml



getNgramsContexts :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId -> TabType -> Maybe Limit
             -> m (HashMap NgramsTerm (Set ContextId))
getNgramsContexts cId maybeListId tabType maybeLimit = do
  (_ngs', ngs) <- getNgrams cId maybeListId tabType

  lId  <- defaultList cId
  lIds <- selectNodesWithUsername NodeList userMaster

  -- TODO maybe add an option to group here
  getContextsByNgramsOnlyUser  cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               (take' maybeLimit $ HM.keys ngs)



-- Used for scores in Doc Table
getContextsNgramsScore :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId -> TabType -> ListType -> Maybe Limit
             -> m (Map ContextId Int)
getContextsNgramsScore cId maybeListId tabType listType maybeLimit 
 = Map.map Set.size <$> getContextsNgrams cId maybeListId tabType listType maybeLimit

getContextsNgrams :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId -> TabType -> ListType -> Maybe Limit
             -> m (Map ContextId (Set NgramsTerm))
getContextsNgrams cId maybeListId tabType listType maybeLimit = do
  (ngs', ngs) <- getNgrams cId maybeListId tabType
  lId  <- defaultList cId
  lIds <- selectNodesWithUsername NodeList userMaster

  result <- groupNodesByNgrams ngs <$> getContextsByNgramsOnlyUser
                               cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               ( take' maybeLimit
                               $ HM.keys
                               $ HM.filter (\v -> fst v == listType) ngs'
                               )

  pure $ Map.fromListWith (<>)
       $ List.concat
       $ map (\(ng, contexts) -> List.zip (Set.toList contexts) (List.cycle [Set.singleton ng]))
       $ HM.toList result



getNgrams :: (HasMail env, HasNodeStory env err m)
            => CorpusId -> Maybe ListId -> TabType
            -> m ( HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                 , HashMap NgramsTerm (Maybe RootTerm)
                 )
getNgrams cId maybeListId tabType = do

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  lists <- mapTermListRoot [lId] (ngramsTypeFromTabType tabType) <$> getRepo' [lId]
  let maybeSyn = HM.unions $ map (\t -> filterListWithRoot t lists)
                                 [MapTerm, StopTerm, CandidateTerm]
  pure (lists, maybeSyn)

-- Some useful Tools
take' :: Maybe Int -> [a] -> [a]
take' Nothing  xs = xs
take' (Just n) xs = take n xs



