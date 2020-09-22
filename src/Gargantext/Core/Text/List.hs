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

import Control.Lens (makeLenses)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (Down(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
-- import Gargantext.API.Ngrams.Tools (getCoocByNgrams', Diagonal(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.Metrics (scored', Scored(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith, getNodesByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))

import Gargantext.Prelude
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Learn (Model(..))
-- import Gargantext.Core.Text.Metrics (takeScored)


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
buildNgramsLists :: HasNodeError err
                 => Lang
                 -> Int
                 -> Int
                 -> StopSize
                 -> UserCorpusId
                 -> MasterCorpusId
                 -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsLists l n m s uCid mCid = do
  ngTerms     <- buildNgramsTermsList l n m s uCid mCid
  othersTerms <- mapM (buildNgramsOthersList uCid identity)
                      [Authors, Sources, Institutes]
  pure $ Map.unions $ othersTerms <> [ngTerms]


buildNgramsOthersList :: UserCorpusId
                      -> (Text -> Text)
                      -> NgramsType
                      -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsOthersList uCid groupIt nt = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt

  let
    listSize = 9
    all'     = List.reverse
             $ List.sortOn (Set.size . snd . snd)
             $ Map.toList ngs

    graphTerms = List.take listSize all'
    candiTerms = List.drop listSize all'

  pure $ Map.unionsWith (<>) [ toElements MapTerm     graphTerms
                             , toElements CandidateTerm candiTerms
                             ]
    where
      toElements nType x =
        Map.fromList [(nt, [ mkNgramsElement (NgramsTerm t) nType Nothing (mSetFromList [])
                           | (t, _ns) <- x
                           ]
                     )]

-- TODO use ListIds
buildNgramsTermsList :: HasNodeError err
                     => Lang
                     -> Int
                     -> Int
                     -> StopSize
                     -> UserCorpusId
                     -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList l n m s uCid mCid = do

-- Computing global speGen score
  allTerms <- Map.toList <$> getTficf uCid mCid NgramsTerms

  -- printDebug "head candidates" (List.take 10 $ allTerms)
  -- printDebug "tail candidates" (List.take 10 $ List.reverse $ allTerms)

  -- First remove stops terms
  let
    -- stopTerms ignored for now (need to be tagged already)
    (_stopTerms, candidateTerms) = List.partition ((isStopTerm s) . fst) allTerms

  -- Grouping the ngrams and keeping the maximum score for label
  let grouped = groupStems'
        $ map (\(t,d) -> let stem = ngramsGroup l n m t
                          in ( stem
                             , GroupedText Nothing t d Set.empty (size t) stem Set.empty
                             )
              ) candidateTerms

      (groupedMono, groupedMult) = Map.partition (\gt -> _gt_size gt < 2) grouped

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
    mapScores f = Map.fromList $ map (\(Scored t g s') -> (t, f (g,s'))) $ scored' mapCooc

    groupsWithScores = catMaybes
                     $ map (\(stem, g)
                             -> case Map.lookup stem mapScores' of
                                 Nothing -> Nothing
                                 Just s'  -> Just $ g { _gt_score = s'}
                            ) $ Map.toList contextsAdded
      where
        mapScores' = mapScores adapt1 -- identity
        adapt1 (s1,s2) = (log' 5 s1, log' 2 s2)
        log' n' x     = 1 + (if x <= 0 then 0 else log $ (10^(n'::Int)) * x)
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



  pure $ Map.fromList [(NgramsTerms, (List.concat $ map toNgramsElement $ termListHead)
                                  <> (List.concat $ map toNgramsElement $ termListTail)
                       )
                      ]

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
type Group = Lang -> Int -> Int -> Text -> Text
type Stem  = Text
type Label = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Label
              , _gt_score    :: !score
              , _gt_group    :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem
              , _gt_nodes    :: !(Set NodeId)
              }
instance Show score => Show (GroupedText score) where
  show (GroupedText _ l s _ _ _ _) = show l <> ":" <> show s

instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _ _)
       (GroupedText _ _ score2 _ _ _ _) = (==) score1 score2

instance (Eq a, Ord a) => Ord (GroupedText a) where
  compare (GroupedText _ _ score1 _ _ _ _)
          (GroupedText _ _ score2 _ _ _ _) = compare score1 score2



-- Lenses Instances
makeLenses 'GroupedText
