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

{-# LANGUAGE QuasiQuotes          #-}

module Gargantext.Database.Action.Metrics
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Database.PostgreSQL.Simple (Query, Only(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Data.Vector (Vector)
import Gargantext.Core (HasDBid(toDBid))
import Gargantext.API.Ngrams.Tools (filterListWithRoot, groupNodesByNgrams, Diagonal(..), getCoocByNgrams, mapTermListRoot, RootTerm, getRepo)
import Gargantext.Database.Prelude (runPGSQuery{-, formatPGSQuery-})
import Gargantext.API.Ngrams.Types (TabType(..), ngramsTypeFromTabType, NgramsTerm(..))
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.NodeStory hiding (runPGSQuery)
import Gargantext.Core.Text.Metrics (scored, Scored(..), {-localMetrics, toScored-})
import Database.PostgreSQL.Simple.ToField (toField, Action{-, ToField-})
import Gargantext.Core.Types (ListType(..), NodeType(..), ContextId)
import Gargantext.Core.Types.Query (Limit(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsOnlyUser{-, getTficfWith-})
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Action.Metrics.NgramsByContext (refreshNgramsMaterialized)
import Gargantext.Database.Admin.Types.Node (ListId, CorpusId)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.List           as List
import qualified Data.Text           as Text

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

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  (ngs', ngs) <- getNgrams lId tabType

  lIds <- selectNodesWithUsername NodeList userMaster

  myCooc <- HM.filter (>1) <$> getCoocByNgrams (Diagonal True)
                           <$> groupNodesByNgrams ngs
                           <$> getContextsByNgramsOnlyUser cId
                                                           (lIds <> [lId])
                                                           (ngramsTypeFromTabType tabType)
                                                           (take' maybeLimit $ HM.keys ngs)
  pure $ (ngs', ngs, myCooc)

------------------------------------------------------------------------
------------------------------------------------------------------------
updateNgramsOccurrences :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId
             -> m ()
updateNgramsOccurrences cId mlId = do
  _ <- mapM (updateNgramsOccurrences' cId mlId Nothing) [Terms, Sources, Authors, Institutes]
  pure ()


updateNgramsOccurrences' :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId -> Maybe Limit -> TabType
             -> m [Int]
updateNgramsOccurrences' cId maybeListId maybeLimit tabType = do

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  result <- getNgramsOccurrences cId lId tabType maybeLimit

  let
    toInsert :: [[Action]]
    toInsert =  map (\(ngramsTerm, score)
                        -> [ toField cId
                           , toField lId
                           , toField $ unNgramsTerm ngramsTerm
                           , toField $ toDBid $ ngramsTypeFromTabType tabType
                           , toField score
                           ]
                      )
       $ HM.toList result

    queryInsert :: Query
    queryInsert = [sql|
                  WITH input(corpus_id, list_id, terms, type_id, weight) AS (?)
                  INSERT into node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                  SELECT input.corpus_id,input.list_id,ngrams.id,input.type_id,input.weight FROM input
                  JOIN ngrams on ngrams.terms = input.terms
                  ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                  DO UPDATE SET weight = excluded.weight
                  RETURNING 1
                  |]

  let fields = map (\t-> QualifiedIdentifier Nothing t)
             $ map Text.pack ["int4", "int4","text","int4","int4"]

  res <- map (\(Only a) -> a) <$> runPGSQuery queryInsert (Only $ Values fields toInsert)

  -- _ <- map (\(Only a) -> a) <$> runPGSQuery [sql|refresh materialized view context_node_ngrams_view;|] ()
  _ <- refreshNgramsMaterialized
  pure res



------------------------------------------------------------------------
-- Used for scores in Ngrams Table
getNgramsOccurrences :: (FlowCmdM env err m)
             => CorpusId -> ListId -> TabType -> Maybe Limit
             -> m (HashMap NgramsTerm Int)
getNgramsOccurrences c l t ml = HM.map Set.size <$> getNgramsContexts c l t ml



getNgramsContexts :: (FlowCmdM env err m)
             => CorpusId -> ListId -> TabType -> Maybe Limit
             -> m (HashMap NgramsTerm (Set ContextId))
getNgramsContexts cId lId tabType maybeLimit = do
  (_ngs', ngs) <- getNgrams lId tabType
  lIds <- selectNodesWithUsername NodeList userMaster

  -- TODO maybe add an option to group here
  getContextsByNgramsOnlyUser  cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               (take' maybeLimit $ HM.keys ngs)



------------------------------------------------------------------------
updateContextScore :: (FlowCmdM env err m)
             => CorpusId -> Maybe ListId
             -> m [Int]
updateContextScore cId maybeListId = do

  lId <- case maybeListId of
    Nothing   -> defaultList cId
    Just lId' -> pure lId'

  result <- getContextsNgramsScore cId lId Terms MapTerm Nothing

  let
    toInsert :: [[Action]]
    toInsert =  map (\(contextId, score)
                        -> [ toField cId
                           , toField contextId
                           , toField score
                           ]
                      )
       $ Map.toList result

    queryInsert :: Query
    queryInsert = [sql|
                  WITH input(node_id, context_id, score) AS (?)
                    UPDATE nodes_contexts nc
                    SET score = input.score
                    FROM input
                    WHERE nc.node_id = input.node_id
                    AND nc.context_id = input.context_id
                    RETURNING 1
                  |]

  let fields = map (\t-> QualifiedIdentifier Nothing t)
             $ map Text.pack ["int4", "int4","int4"]

  map (\(Only a) -> a) <$> runPGSQuery queryInsert (Only $ Values fields toInsert)




-- Used for scores in Doc Table
getContextsNgramsScore :: (FlowCmdM env err m)
             => CorpusId -> ListId -> TabType -> ListType -> Maybe Limit
             -> m (Map ContextId Int)
getContextsNgramsScore cId lId tabType listType maybeLimit
 = Map.map Set.size <$> getContextsNgrams cId lId tabType listType maybeLimit

getContextsNgrams :: (FlowCmdM env err m)
             => CorpusId -> ListId -> TabType -> ListType -> Maybe Limit
             -> m (Map ContextId (Set NgramsTerm))
getContextsNgrams cId lId tabType listType maybeLimit = do
  (ngs', ngs) <- getNgrams lId tabType
  lIds <- selectNodesWithUsername NodeList userMaster

  result <- groupNodesByNgrams ngs <$> getContextsByNgramsOnlyUser
                               cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               ( take' maybeLimit
                               $ HM.keys
                               $ HM.filter (\v -> fst v == listType) ngs'
                               )
  -- printDebug "getCoocByNgrams" result
  pure $ Map.fromListWith (<>)
       $ List.concat
       $ map (\(ng, contexts) -> List.zip (Set.toList contexts) (List.cycle [Set.singleton ng]))
       $ HM.toList result


------------------------------------------------------------------------
------------------------------------------------------------------------


getNgrams :: (HasMail env, HasNodeStory env err m)
            => ListId -> TabType
            -> m ( HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                 , HashMap NgramsTerm (Maybe RootTerm)
                 )
getNgrams lId tabType = do

  lists <- mapTermListRoot [lId] (ngramsTypeFromTabType tabType) <$> getRepo [lId]
  -- TODO filterListWithRoot [MapTerm, StopTerm, CandidateTerm] lists
  let maybeSyn = HM.unions $ map (\t -> filterListWithRoot t lists)
                                 [[MapTerm], [StopTerm], [CandidateTerm]]
  pure (lists, maybeSyn)

-- Some useful Tools
take' :: Maybe Limit -> [a] -> [a]
take' Nothing  xs = xs
take' (Just n) xs = take (getLimit n) xs
