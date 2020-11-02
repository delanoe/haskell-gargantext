{-|
Module      : Gargantext.Core.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Text.List
  where


import Control.Lens ((^.), set)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (Down(..))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

-- import Gargantext.API.Ngrams.Tools (getCoocByNgrams', Diagonal(..))
import Gargantext.API.Ngrams.Types (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
import Gargantext.API.Ngrams.Types (RepoCmdM)
import Gargantext.Core.Text.List.Social (flowSocialList, invertForw)
import Gargantext.Core.Text.Metrics (scored', Scored(..), normalizeGlobal, normalizeLocal)
import Gargantext.Core.Text.Group
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsUser, groupNodesByNgramsWith, getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Database.Prelude (CmdM)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude


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
  ngTerms     <- buildNgramsTermsList user uCid mCid gp
  othersTerms <- mapM (buildNgramsOthersList user uCid (ngramsGroup GroupIdentity))
                      [ (Authors   , MapListSize 9)
                      , (Sources   , MapListSize 9)
                      , (Institutes, MapListSize 9)
                      ]

  pure $ Map.unions $ [ngTerms] <> othersTerms


data MapListSize = MapListSize Int

buildNgramsOthersList ::( HasNodeError err
                        , CmdM     env err m
                        , RepoCmdM env err m
                        , HasTreeError err
                        )
                        => User
                        -> UserCorpusId
                        -> (Text -> Text)
                        -> (NgramsType, MapListSize)
                        -> m (Map NgramsType [NgramsElement])
buildNgramsOthersList user uCid groupIt (nt, MapListSize mapListSize) = do

  ngs  <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt


  let 
    grouped = toGroupedText groupIt (Set.size . snd) fst snd
              (Map.toList $ Map.mapWithKey (\k (a,b) -> (Set.delete k a, b)) $ ngs)

  socialLists <- flowSocialList user nt (Set.fromList $ Map.keys ngs)

  let
    groupedWithList        = map (addListType (invertForw socialLists)) grouped
    (stopTerms, tailTerms) = Map.partition (\t -> t ^. gt_listType == Just StopTerm) groupedWithList
    (mapTerms, tailTerms') = Map.partition (\t -> t ^. gt_listType == Just MapTerm) tailTerms

    listSize = mapListSize - (List.length mapTerms)
    (mapTerms', candiTerms) = List.splitAt listSize $ List.sortOn (Down . _gt_score) $ Map.elems tailTerms'

  pure $ Map.fromList [( nt, (List.concat $ map toNgramsElement stopTerms)
                                 <> (List.concat $ map toNgramsElement mapTerms)
                                 <> (List.concat $ map toNgramsElement $ map (set gt_listType (Just MapTerm)) mapTerms')
                                 <> (List.concat $ map toNgramsElement $ map (set gt_listType (Just CandidateTerm)) candiTerms)
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
                        -> m (Map NgramsType [NgramsElement])
buildNgramsTermsList user uCid mCid groupParams = do

-- Computing global speGen score
  allTerms <- Map.toList <$> getTficf uCid mCid NgramsTerms

  -- printDebug "head candidates" (List.take 10 $ allTerms)
  -- printDebug "tail candidates" (List.take 10 $ List.reverse $ allTerms)

  -- First remove stops terms
  socialLists <- flowSocialList user NgramsTerms (Set.fromList $ map fst allTerms)
  -- printDebug "\n * socialLists * \n" socialLists

  printDebug "\n * socialLists * \n" socialLists

  let
    _socialStop = fromMaybe Set.empty $ Map.lookup StopTerm      socialLists
    _socialMap  = fromMaybe Set.empty $ Map.lookup MapTerm       socialLists
    _socialCand = fromMaybe Set.empty $ Map.lookup CandidateTerm socialLists
    -- stopTerms ignored for now (need to be tagged already)
    -- (stopTerms, candidateTerms) = List.partition ((\t -> Set.member t socialStop) . fst) allTerms
    -- (mapTerms,  candidateTerms) = List.partition ((\t -> Set.member t socialMap ) . fst) allTerms

  -- printDebug "stopTerms" stopTerms

  -- Grouping the ngrams and keeping the maximum score for label
  let grouped = toGroupedText (ngramsGroup groupParams) identity (const Set.empty) (const Set.empty) allTerms

      groupedWithList = map (addListType (invertForw socialLists)) grouped

      (stopTerms, candidateTerms) = Map.partition (\t -> t ^. gt_listType == Just StopTerm) groupedWithList
      (groupedMono, groupedMult)  = Map.partition (\t -> t ^. gt_size                  < 2) candidateTerms

  -- printDebug "\n * stopTerms * \n" stopTerms
  -- splitting monterms and multiterms to take proportional candidates
  let
    listSizeGlobal = 2000 :: Double -- use % of list if to big, or Int if to small
    monoSize = 0.4  :: Double
    multSize = 1 - monoSize

    splitAt n' ns = List.splitAt (round $ n' * listSizeGlobal) $ List.sort $ Map.elems ns

    (groupedMonoHead, groupedMonoTail) = splitAt monoSize groupedMono
    (groupedMultHead, groupedMultTail) = splitAt multSize groupedMult

  -- printDebug "groupedMonoHead" (List.length groupedMonoHead)
  -- printDebug "groupedMonoTail" (List.length groupedMonoHead)
  -- printDebug "groupedMultHead" (List.length groupedMultHead)
  -- printDebug "groupedMultTail" (List.length groupedMultTail)

  let
    -- Get Local Scores now for selected grouped ngrams
    selectedTerms = Set.toList $ List.foldl'
                      (\set' (GroupedText _ l' _ g _ _ _ ) -> Set.union set'
                                                            $ Set.insert l' g
                      )
                      Set.empty
                      (groupedMonoHead <> groupedMultHead)

  -- TO remove (and remove HasNodeError instance)
  userListId    <- defaultList uCid
  masterListId  <- defaultList mCid

  mapTextDocIds <- getNodesByNgramsOnlyUser uCid [userListId, masterListId] NgramsTerms selectedTerms

  let
    mapGroups   = Map.fromList
                $ map (\g -> (g ^. gt_stem, g))
                $ groupedMonoHead <> groupedMultHead

    -- grouping with Set NodeId
    contextsAdded = foldl' (\mapGroups' k -> let k' = ngramsGroup groupParams k
                                    in case Map.lookup k' mapGroups'  of
                                      Nothing -> mapGroups'
                                      Just g  -> case Map.lookup k mapTextDocIds of
                                                   Nothing -> mapGroups'
                                                   Just ns -> Map.insert k' ( g { _gt_nodes = Set.union ns (_gt_nodes g)}) mapGroups'
                           )
                  mapGroups
                  $ Map.keys mapTextDocIds

    -- compute cooccurrences
    mapCooc = Map.filter (>2)
            $ Map.fromList [ ((t1, t2), Set.size $ Set.intersection s1 s2)
                           | (t1, s1) <- mapStemNodeIds
                           , (t2, s2) <- mapStemNodeIds
                           ]
      where
        mapStemNodeIds = Map.toList $ Map.map (_gt_nodes) contextsAdded
  -- printDebug "mapCooc" mapCooc

  let
    -- computing scores
    mapScores f = Map.fromList
                $ map (\(Scored t g s') -> (t, f (g,s')))
                $ normalizeGlobal
                $ map normalizeLocal
                $ scored' mapCooc

    groupsWithScores = catMaybes
                     $ map (\(stem, g)
                             -> case Map.lookup stem mapScores' of
                                 Nothing -> Nothing
                                 Just s'  -> Just $ g { _gt_score = s'}
                            ) $ Map.toList contextsAdded
      where
        mapScores' = mapScores identity
        -- adapt2 TOCHECK with DC
  -- printDebug "groupsWithScores" groupsWithScores
  let
    -- sort / partition / split
      -- filter mono/multi again
    (monoScored, multScored) = List.partition (\g -> _gt_size g < 2) groupsWithScores
      -- filter with max score
    partitionWithMaxScore = List.partition (\g -> let (s1,s2) = _gt_score g in s1 > s2 )

    (monoScoredIncl, monoScoredExcl) = partitionWithMaxScore monoScored
    (multScoredIncl, multScoredExcl) = partitionWithMaxScore multScored

      -- splitAt
  let
    listSizeLocal = 1000 :: Double -- use % of list if to big, or Int if to small
    inclSize = 0.4  :: Double
    exclSize = 1 - inclSize
    splitAt' n' = List.splitAt (round $ n' * listSizeLocal)

    (monoScoredInclHead, monoScoredInclTail) = splitAt' (monoSize * inclSize / 2) $ List.sortOn (Down . _gt_score) monoScoredIncl
    (monoScoredExclHead, monoScoredExclTail) = splitAt' (monoSize * inclSize / 2) $ List.sortOn (Down . _gt_score) monoScoredExcl

    (multScoredInclHead, multScoredInclTail) = splitAt' (multSize * exclSize / 2) $ List.sortOn (Down . _gt_score) multScoredIncl
    (multScoredExclHead, multScoredExclTail) = splitAt' (multSize * exclSize / 2) $ List.sortOn (Down . _gt_score) multScoredExcl


    -- Final Step building the Typed list
    termListHead = maps <> cands
      where
        maps = set gt_listType (Just MapTerm)
            <$> monoScoredInclHead
             <> monoScoredExclHead
             <> multScoredInclHead
             <> multScoredExclHead

        cands = set gt_listType (Just CandidateTerm)
             <$> monoScoredInclTail
              <> monoScoredExclTail
              <> multScoredInclTail
              <> multScoredExclTail

    termListTail = map (set gt_listType (Just CandidateTerm)) ( groupedMonoTail <> groupedMultTail)

--  printDebug "monoScoredInclHead" monoScoredInclHead
--  printDebug "monoScoredExclHead" monoScoredExclTail
--
--  printDebug "multScoredInclHead" multScoredInclHead
--  printDebug "multScoredExclTail" multScoredExclTail

  let result = Map.unionsWith (<>)
       [ Map.fromList [( NgramsTerms, (List.concat $ map toNgramsElement $ termListHead)
                                   <> (List.concat $ map toNgramsElement $ termListTail)
                                   <> (List.concat $ map toNgramsElement $ stopTerms)
                      )]
       ]
  -- printDebug "\n result \n" r
  pure result



toNgramsElement :: GroupedText a -> [NgramsElement]
toNgramsElement (GroupedText listType label _ setNgrams _ _ _) =
  [parentElem] <> childrenElems
    where
      parent = label
      children = Set.toList setNgrams
      parentElem    = mkNgramsElement (NgramsTerm parent)
                                      (fromMaybe CandidateTerm listType)
                                      Nothing
                                      (mSetFromList (NgramsTerm <$> children))
      childrenElems = map (\t -> mkNgramsElement t (fromMaybe CandidateTerm $ listType)
                                                 (Just $ RootParent (NgramsTerm parent) (NgramsTerm parent))
                                                 (mSetFromList [])
                          ) (NgramsTerm <$> children)


toGargList :: Maybe ListType -> b -> (Maybe ListType, b)
toGargList l n = (l,n)


isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)

------------------------------------------------------------------------------
