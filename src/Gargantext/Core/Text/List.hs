{-|
Module      : Gargantext.Core.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.List
  where

-- import Data.Either (partitionEithers, Either(..))
import Data.Maybe (fromMaybe)
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
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId, Ordering(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Core.Text.Metrics.TFICF (sortTficf)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))

import Gargantext.Prelude
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Learn (Model(..))
-- import Gargantext.Core.Text.Metrics (takeScored)


data NgramsListBuilder = BuilderStepO { stemSize :: Int
                                      , stemX    :: Int
                                      , stopSize :: Int
                                      }
                       | BuilderStep1 { withModel :: Model }
                       | BuilderStepN { withModel :: Model }
                       | Tficf { nlb_lang           :: Lang
                               , nlb_group1         :: Int
                               , nlb_group2         :: Int
                               , nlb_stopSize       :: StopSize
                               , nlb_userCorpusId   :: UserCorpusId
                               , nlb_masterCorpusId :: MasterCorpusId
                               }


data StopSize = StopSize {unStopSize :: Int}

-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: Lang
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

buildNgramsTermsList :: Lang
                     -> Int
                     -> Int
                     -> StopSize
                     -> UserCorpusId
                     -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList l n m s uCid mCid = do

-- Computing global speGen score
-- TODO sort is not needed, just take the score
  allTerms <- sortTficf Up <$> getTficf uCid mCid NgramsTerms
  -- printDebug "head candidates" (List.take 10 $ allTerms)
  -- printDebug "tail candidates" (List.take 10 $ List.reverse $ allTerms)

  -- First remove stops terms
  let 
    (stopTerms, candidateTerms) = List.partition ((isStopTerm s) . fst) allTerms

  -- Grouping the ngrams and keeping the maximum score for label
  let grouped = groupStems'
        $ map (\(t,d) -> let stem = ngramsGroup l n m t
                          in ( stem
                             , GroupedText Nothing t d Set.empty (size t) stem
                             )
              ) candidateTerms

      (groupedMono, groupedMult) = Map.partition (\gt -> _gt_size gt < 2) grouped

  -- splitting monterms and multiterms to take proportional candidates
  let
    listSizeGlobal = 2000 :: Double -- use % of list if to big, or Int if to small
    monoSizeGlobal = 0.6  :: Double
    multSizeGlobal = 1 - monoSizeGlobal

    splitAt n ns = List.splitAt (round $ n * listSizeGlobal) $ List.sort $ Map.elems ns

    (groupedMonoHead, groupedMonoTail) = splitAt monoSizeGlobal groupedMono
    (groupedMultHead, groupedMultTail) = splitAt multSizeGlobal groupedMult

    -- Get Local Scores now for selected grouped ngrams
    selectedTerms = Set.toList $ List.foldl'
                      (\set (GroupedText _ l _ g _ _) -> Set.union set $ Set.union g $ Set.singleton l)
                      Set.empty
                      (groupedMonoHead <> groupedMultHead)



    (mono, multi)          = List.partition (\t -> (size . fst) t < 2) candidateTerms
    (monoHead , monoTail ) = List.splitAt (round $ 0.60 * listSizeGlobal) mono
    (multiHead, multiTail) = List.splitAt (round $ 0.40 * listSizeGlobal) multi


  -- Computing local speGen score
    listSizeLocal  = 350  :: Double

    -- Final Step building the Typed list
    termList = (map (toGargList $ Just StopTerm) stopTerms)
            <> (map (toGargList $ Just MapTerm)       (monoHead <> multiHead))
            <> (map (toGargList $ Just CandidateTerm) (monoTail <> multiTail))

    ngs = List.concat
        $ map toNgramsElement
        $ groupStems
        $ map (\(listType, (t,d)) -> let stem = ngramsGroup l n m t
                                      in ( stem
                                         , GroupedText listType t d Set.empty (size t) stem
                                         )
              ) termList

  pure $ Map.fromList [(NgramsTerms, ngs)]

type Group = Lang -> Int -> Int -> Text -> Text
type Stem  = Text
type Label = Text
data GroupedText score =
  GroupedText { _gt_listType :: Maybe ListType
              , _gt_label    :: Label
              , _gt_score    :: score
              , _gt_group    :: Set Text
              , _gt_size     :: Int
              , _gt_stem     :: Stem
              }

instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _)
       (GroupedText _ _ score2 _ _ _) = (==) score1 score2

instance (Eq a, Ord a) => Ord (GroupedText a) where
  compare (GroupedText _ _ score1 _ _ _)
          (GroupedText _ _ score2 _ _ _) = compare score1 score2

groupStems :: [(Stem, GroupedText Double)] -> [GroupedText Double]
groupStems = Map.elems . groupStems'

groupStems' :: [(Stem, GroupedText Double)] -> Map Stem (GroupedText Double)
groupStems' = Map.fromListWith grouping
  where
    grouping (GroupedText lt1 label1 score1 group1 s1 stem1)
             (GroupedText lt2 label2 score2 group2 s2 stem2)
             | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr) s1 stem1
             | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr) s2 stem2
        where
          lt = lt1 <> lt2
          gr = Set.union group1 group2




toNgramsElement :: GroupedText Double -> [NgramsElement]
toNgramsElement (GroupedText listType label _ setNgrams _ _) =
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
