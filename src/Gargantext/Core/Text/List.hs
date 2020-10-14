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


import Control.Lens ((^.))
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
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social (flowSocialList, invertForw)
import Gargantext.Core.Text.Metrics (scored', Scored(..), normalizeGlobal, normalizeLocal)
import Gargantext.Core.Text.Types
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith, getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Database.Prelude (Cmd, CmdM)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude


data NgramsListBuilder = BuilderStepO { stemSize :: !Int
                                      , stemX    :: !Int
                                      , stopSize :: !Int
                                      }
                       | BuilderStep1 { withModel :: !Model }
                       | BuilderStepN { withModel :: !Model }
                       | Tficf { nlb_lang           :: !Lang
                               , nlb_group1         :: !Int
                               , nlb_group2         :: !Int
                               , nlb_stopSize       :: !StopSize
                               , nlb_userCorpusId   :: !UserCorpusId
                               , nlb_masterCorpusId :: !MasterCorpusId
                               }


data StopSize = StopSize {unStopSize :: !Int}

-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: ( RepoCmdM env err m
                    , CmdM     env err m
                    , HasTreeError err
                    , HasNodeError err
                    )
                 => User
                 -> Lang
                 -> Int
                 -> Int
                 -> StopSize
                 -> UserCorpusId
                 -> MasterCorpusId
                 -> m (Map NgramsType [NgramsElement])
buildNgramsLists user l n m s uCid mCid = do
  ngTerms     <- buildNgramsTermsList user l n m s uCid mCid
  othersTerms <- mapM (buildNgramsOthersList user uCid identity)
                      [Authors, Sources, Institutes]
  pure $ Map.unions $ othersTerms <> [ngTerms]


buildNgramsOthersList :: (--  RepoCmdM env err m
                        -- , CmdM     env err m
                         HasNodeError err
                        -- , HasTreeError err
                        )
                        => User
                      -> UserCorpusId
                      -> (Text -> Text)
                      -> NgramsType
                      -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsOthersList _user uCid groupIt nt = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt

  let
    listSize = 9
    all'     = List.sortOn (Down . Set.size . snd . snd)
             $ Map.toList ngs

    (graphTerms, candiTerms) = List.splitAt listSize all'

  pure $ Map.unionsWith (<>) [ toElements nt MapTerm       graphTerms
                             , toElements nt CandidateTerm candiTerms
                             ]

toElements :: Ord k => k -> ListType -> [(Text, b)] -> Map k [NgramsElement]
toElements nType lType x =
  Map.fromList [(nType, [ mkNgramsElement (NgramsTerm t) lType Nothing (mSetFromList [])
                        | (t, _ns) <- x
                        ]
               )]

-- TODO use ListIds
buildNgramsTermsList :: ( HasNodeError err
                        , CmdM     env err m
                        , RepoCmdM env err m
                        , HasTreeError err
                        )
                        => User
                        -> Lang
                        -> Int
                        -> Int
                        -> StopSize
                        -> UserCorpusId
                        -> MasterCorpusId
                        -> m (Map NgramsType [NgramsElement])
buildNgramsTermsList user l n m _s uCid mCid = do

-- Computing global speGen score
  allTerms <- Map.toList <$> getTficf uCid mCid NgramsTerms

  -- printDebug "head candidates" (List.take 10 $ allTerms)
  -- printDebug "tail candidates" (List.take 10 $ List.reverse $ allTerms)

  -- First remove stops terms
  socialLists <- flowSocialList user NgramsTerms (Set.fromList $ map fst allTerms)

  printDebug "\n * socialLists * \n" socialLists

{-
  let
    _socialMap  = fromMaybe Set.empty $ Map.lookup MapTerm       socialLists
    _socialCand = fromMaybe Set.empty $ Map.lookup CandidateTerm socialLists
    socialStop  = fromMaybe Set.empty $ Map.lookup StopTerm      socialLists
    -- stopTerms ignored for now (need to be tagged already)
    -- (stopTerms, candidateTerms) = List.partition ((\t -> Set.member t socialStop) . fst) allTerms
-}


  -- Grouping the ngrams and keeping the maximum score for label
  let grouped = groupStems'
        $ map (\(t,d) -> let stem = ngramsGroup l n m t
                          in ( stem
                             , GroupedText Nothing t d Set.empty (size t) stem Set.empty
                             )
              ) allTerms

      groupedWithList = map (addListType (invertForw socialLists)) grouped
      (stopTerms, candidateTerms) = Map.partition (\t -> t ^. gt_listType == Just StopTerm) groupedWithList
      (groupedMono, groupedMult)  = Map.partition (\gt -> _gt_size gt < 2) candidateTerms

  -- printDebug "\n * stopTerms * \n" stopTerms
-- printDebug "groupedMult" groupedMult
  -- splitting monterms and multiterms to take proportional candidates
  let
    listSizeGlobal = 2000 :: Double -- use % of list if to big, or Int if to small
    monoSize = 0.4  :: Double
    multSize = 1 - monoSize

    splitAt n' ns = List.splitAt (round $ n' * listSizeGlobal) $ List.sort $ Map.elems ns
 
    (groupedMonoHead, groupedMonoTail) = splitAt monoSize groupedMono
    (groupedMultHead, groupedMultTail) = splitAt multSize groupedMult

  printDebug "groupedMonoHead" (List.length groupedMonoHead)
  printDebug "groupedMonoTail" (List.length groupedMonoHead)
  printDebug "groupedMultHead" (List.length groupedMultHead)
  printDebug "groupedMultTail" (List.length groupedMultTail)

  let
    -- Get Local Scores now for selected grouped ngrams
    selectedTerms = Set.toList $ List.foldl'
                      (\set' (GroupedText _ l' _ g _ _ _ ) -> Set.union set'
                                                            $ Set.union g
                                                            $ Set.singleton l'
                      )
                      Set.empty
                      (groupedMonoHead <> groupedMultHead)

  -- TO remove (and remove HasNodeError instance)
  userListId    <- defaultList uCid
  masterListId  <- defaultList mCid

  mapTextDocIds <- getNodesByNgramsOnlyUser uCid [userListId, masterListId] NgramsTerms selectedTerms
  let
    mapGroups   = Map.fromList
                $ map (\g -> (_gt_stem g, g))
                $ groupedMonoHead <> groupedMultHead

    -- grouping with Set NodeId
    contextsAdded = foldl' (\mapGroups' k -> let k' = ngramsGroup l n m k
                                    in case Map.lookup k' mapGroups'  of
                                      Nothing -> mapGroups'
                                      Just g  -> case Map.lookup k mapTextDocIds of
                                                   Nothing -> mapGroups'
                                                   Just ns -> Map.insert k' ( g { _gt_nodes = Set.union ns (_gt_nodes g)}) mapGroups'
                           )
                  mapGroups
                  $ Map.keys mapTextDocIds

    -- compute cooccurrences
    mapCooc = Map.filter (>2) $ Map.fromList [ ((t1, t2), Set.size $ Set.intersection s1 s2)
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
    -- (map (toGargList $ Just StopTerm) stopTerms) -- Removing stops (needs social score)
    termListHead =
             (map (\g -> g { _gt_listType = Just MapTerm} )  (  monoScoredInclHead
                                                 <> monoScoredExclHead
                                                 <> multScoredInclHead
                                                 <> multScoredExclHead
                                                 )
               )
            <> (map (\g -> g { _gt_listType = Just CandidateTerm }) (  monoScoredInclTail
                                                      <> monoScoredExclTail
                                                      <> multScoredInclTail
                                                      <> multScoredExclTail
                                                      )
               )

    termListTail = map (\g -> g { _gt_listType = Just CandidateTerm }) ( groupedMonoTail <> groupedMultTail)

--  printDebug "monoScoredInclHead" monoScoredInclHead
--  printDebug "monoScoredExclHead" monoScoredExclTail
--
  printDebug "multScoredInclHead" multScoredInclHead
  printDebug "multScoredExclTail" multScoredExclTail

  let result = Map.unionsWith (<>)
       [ Map.fromList [(
                        NgramsTerms, (List.concat $ map toNgramsElement $ termListHead)
                                  <> (List.concat $ map toNgramsElement $ termListTail)
                                  <> (List.concat $ map toNgramsElement $ stopTerms)
                      )]
       ]
  -- printDebug "\n result \n" r
  pure result

groupStems :: [(Stem, GroupedText Double)] -> [GroupedText Double]
groupStems = Map.elems . groupStems'

groupStems' :: [(Stem, GroupedText Double)] -> Map Stem (GroupedText Double)
groupStems' = Map.fromListWith grouping
  where
    grouping (GroupedText lt1 label1 score1 group1 s1 stem1 nodes1)
             (GroupedText lt2 label2 score2 group2 s2 stem2 nodes2)
             | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr) s1 stem1 nodes
             | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr) s2 stem2 nodes
        where
          lt = lt1 <> lt2
          gr    = Set.union group1 group2
          nodes = Set.union nodes1 nodes2




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
