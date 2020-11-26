{-|
Module      : Gargantext.Core.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.Core.Text.List
  where

import Control.Lens hiding (both) -- ((^.), view, over, set, (_1), (_2))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mempty)
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (both)
import Gargantext.API.Ngrams.Types (NgramsElement)
import Gargantext.API.Ngrams.Types (RepoCmdM)
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Group
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Social
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.Metrics (scored', Scored(..), scored_speExc, scored_genInc, normalizeGlobal, normalizeLocal, scored_terms)
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsUser, getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Prelude (CmdM)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text


{-
-- TODO maybe useful for later
isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
-}



-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: ( RepoCmdM env err m
                    , CmdM     env err m
                    , HasTreeError err
                    , HasNodeError err
                    )
                 => User
                 -> GroupParams
                 -> UserCorpusId
                 -> MasterCorpusId
                 -> m (Map NgramsType [NgramsElement])
buildNgramsLists user gp uCid mCid = do
  ngTerms     <- buildNgramsTermsList user uCid mCid gp (NgramsTerms, MapListSize 350)
  othersTerms <- mapM (buildNgramsOthersList user uCid GroupIdentity)
                      [ (Authors   , MapListSize 9)
                      , (Sources   , MapListSize 9)
                      , (Institutes, MapListSize 9)
                      ]

  pure $ Map.unions $ [ngTerms] <> othersTerms


data MapListSize = MapListSize { unMapListSize :: !Int }

buildNgramsOthersList ::( HasNodeError err
                        , CmdM     env err m
                        , RepoCmdM env err m
                        , HasTreeError err
                        )
                        => User
                        -> UserCorpusId
                        -> GroupParams
                        -> (NgramsType, MapListSize)
                        -> m (Map NgramsType [NgramsElement])
buildNgramsOthersList user uCid groupParams (nt, MapListSize mapListSize) = do
  allTerms  :: Map Text (Set NodeId) <- getNodesByNgramsUser uCid nt

  -- | PrivateFirst for first developments since Public NodeMode is not implemented yet
  socialLists' :: FlowCont Text FlowListScores
    <- flowSocialList' MySelfFirst user nt ( FlowCont Map.empty
                                                      $ Map.fromList
                                                      $ List.zip (Map.keys allTerms) 
                                                                 (List.cycle [mempty])
                                           )
  let
    groupedWithList = toGroupedTree groupParams socialLists' allTerms

  let
    (stopTerms, tailTerms) = Map.partition ((== Just StopTerm) . viewListType) $ view flc_scores groupedWithList
    (mapTerms, tailTerms') = Map.partition ((== Just MapTerm)  . viewListType) tailTerms

    listSize = mapListSize - (List.length mapTerms)
    (mapTerms', candiTerms) = both Map.fromList
                            $ List.splitAt listSize
                            $ List.sortOn (Down . viewScore . snd)
                            $ Map.toList tailTerms'

  pure $ Map.fromList [( nt,  (toNgramsElement stopTerms)
                           <> (toNgramsElement mapTerms )
                           <> (toNgramsElement $ setListType (Just MapTerm      ) mapTerms' )
                           <> (toNgramsElement $ setListType (Just CandidateTerm) candiTerms)
                      )]


-- TODO use ListIds
buildNgramsTermsList :: ( HasNodeError err
                        , CmdM     env err m
                        , RepoCmdM env err m
                        , HasTreeError err
                        )
                        => User
                        -> UserCorpusId
                        -> MasterCorpusId
                        -> GroupParams
                        -> (NgramsType, MapListSize)
                        -> m (Map NgramsType [NgramsElement])
buildNgramsTermsList user uCid mCid groupParams (nt, mapListSize)= do

-- Computing global speGen score
  allTerms :: Map Text Double <- getTficf uCid mCid nt

  -- | PrivateFirst for first developments since Public NodeMode is not implemented yet
  socialLists' :: FlowCont Text FlowListScores
    <- flowSocialList' MySelfFirst user nt ( FlowCont Map.empty
                                                      $ Map.fromList
                                                      $ List.zip (Map.keys allTerms) 
                                                                 (List.cycle [mempty])
                                           )
  let groupedWithList = toGroupedTree groupParams socialLists' allTerms
      (stopTerms, candidateTerms) = Map.partition ((== Just StopTerm) . viewListType)
                                  $ view flc_scores groupedWithList
      (groupedMono, groupedMult)  = Map.partitionWithKey (\t _v -> size t < 2) candidateTerms

  -- splitting monterms and multiterms to take proportional candidates
  let
    -- use % of list if to big, or Int if too small
    listSizeGlobal = 2000 :: Double
    monoSize = 0.4  :: Double
    multSize = 1 - monoSize

    splitAt n' ns = both (Map.fromListWith (<>))
                  $ List.splitAt (round $ n' * listSizeGlobal)
                  $ List.sortOn (viewScore . snd)
                  $ Map.toList ns

    (groupedMonoHead, groupedMonoTail) = splitAt monoSize groupedMono
    (groupedMultHead, groupedMultTail) = splitAt multSize groupedMult

    selectedTerms = Set.toList $ hasTerms (groupedMonoHead <> groupedMultHead)
 
 -- TO remove (and remove HasNodeError instance)
  userListId    <- defaultList uCid
  masterListId  <- defaultList mCid

  mapTextDocIds <- getNodesByNgramsOnlyUser uCid
                                            [userListId, masterListId]
                                            nt
                                            selectedTerms

  let
    groupedTreeScores_SetNodeId :: Map Text (GroupedTreeScores (Set NodeId))
    groupedTreeScores_SetNodeId = undefined
    -- setScoresWith (\_ _ -> mempty) (groupedMonoHead <> groupedMultHead)
    -- groupedTreeScores_SetNodeId = setScoresWith ((fromMaybe mempty) . ((flip Map.lookup) mapTextDocIds)) (groupedMonoHead <> groupedMultHead)

  -- | Coocurrences computation
  --, t1 >= t2 -- permute byAxis diag  -- since matrix symmetric
  let mapCooc = Map.filter (>2)
            $ Map.fromList [ ((t1, t2), Set.size $ Set.intersection s1 s2)
                           | (t1, s1) <- mapStemNodeIds
                           , (t2, s2) <- mapStemNodeIds
                           ]
          where
            mapStemNodeIds = Map.toList
                           $ Map.map viewScores
                           $ groupedTreeScores_SetNodeId
  let
    -- computing scores
    mapScores f = Map.fromList
                $ map (\g -> (view scored_terms g, f g))
                $ normalizeGlobal
                $ map normalizeLocal
                $ scored' mapCooc

  let
    groupedTreeScores_SpeGen :: Map Text (GroupedTreeScores (Scored Double))
    groupedTreeScores_SpeGen = undefined
    -- setScoresWith (\k v -> set gts'_score (Scored "" 0 0) v) (groupedMonoHead <> groupedMultHead)
    -- groupedTreeScores_SpeGen = setScoresWith (\k v -> set gts'_score (fromMaybe (Scored "" 0 0) $ Map.lookup k (mapScores identity)) v) (groupedMonoHead <> groupedMultHead)

  let
    -- sort / partition / split
    -- filter mono/multi again
    (monoScored, multScored) = Map.partitionWithKey (\t _v -> size t < 2) groupedTreeScores_SpeGen
    -- (monoScored, multScored) = List.partition (\g -> _gt_size g < 2) groupsWithScores

      -- filter with max score
    partitionWithMaxScore = Map.partition (\g -> (view scored_genInc $ view gts'_score g) 
                                               > (view scored_speExc $ view gts'_score g)
                                          )

    (monoScoredIncl, monoScoredExcl) = partitionWithMaxScore monoScored
    (multScoredIncl, multScoredExcl) = partitionWithMaxScore multScored

  -- splitAt
  let
    -- use % of list if to big, or Int if to small
    listSizeLocal = 1000 :: Double
    inclSize = 0.4  :: Double
    exclSize = 1 - inclSize

    splitAt' n' = (both (Map.fromList)) . (List.splitAt (round $ n' * listSizeLocal)) 
    sortOn   f  = (List.sortOn (Down . f . _gts'_score . snd)) . Map.toList 

    monoInc_size = monoSize * inclSize / 2
    (monoScoredInclHead, monoScoredInclTail) = splitAt' monoInc_size $ (sortOn _scored_genInc) monoScoredIncl
    (monoScoredExclHead, monoScoredExclTail) = splitAt' monoInc_size $ (sortOn _scored_speExc) monoScoredExcl

    multExc_size = multSize * exclSize / 2
    (multScoredInclHead, multScoredInclTail) = splitAt' multExc_size $ (sortOn _scored_genInc) multScoredIncl
    (multScoredExclHead, multScoredExclTail) = splitAt' multExc_size $ (sortOn _scored_speExc) multScoredExcl

    -- Final Step building the Typed list
    termListHead = maps <> cands
      where
        maps = setListType (Just MapTerm)
            $ monoScoredInclHead
             <> monoScoredExclHead
             <> multScoredInclHead
             <> multScoredExclHead

        cands = setListType (Just CandidateTerm)
             $ monoScoredInclTail
              <> monoScoredExclTail
              <> multScoredInclTail
              <> multScoredExclTail

    termListTail = (setListType (Just CandidateTerm)) (groupedMonoTail <> groupedMultTail)

  let result = Map.unionsWith (<>)
       [ Map.fromList [( nt, toNgramsElement termListHead
                          <> toNgramsElement termListTail
                          <> toNgramsElement stopTerms
                      )]
       ]

  pure result

------------------------------------------------------------------------------
