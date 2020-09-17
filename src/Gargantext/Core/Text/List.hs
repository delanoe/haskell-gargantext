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
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import Gargantext.Prelude

import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
-- import Gargantext.API.Ngrams.Tools (getCoocByNgrams', Diagonal(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId, Ordering(..))
import Gargantext.Database.Action.Metrics.NgramsByNode (ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Core.Text.Metrics.TFICF (sortTficf)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
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
  othersTerms <- mapM (buildNgramsOthersList uCid identity) [Authors, Sources, Institutes]
  pure $ Map.unions $ othersTerms <> [ngTerms]


buildNgramsOthersList :: UserCorpusId
                      -> (Text -> Text)
                      -> NgramsType
                      -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsOthersList uCid groupIt nt = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt

  let
    listSize = 9
    all'     = List.reverse $ List.sortOn (Set.size . snd . snd) $ Map.toList ngs

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
  candidates <- sortTficf Up <$> getTficf uCid mCid NgramsTerms
  -- printDebug "head candidates" (List.take 10 $ candidates)
  -- printDebug "tail candidates" (List.take 10 $ List.reverse $ candidates)

  let
    (candidatesHead, candidatesTail0)    = List.splitAt 3 candidates
    (candidatesMap, candidatesTailFinal) = List.splitAt 400 candidatesTail0

    termList = (map (toGargList ((isStopTerm s) . fst) CandidateTerm) candidatesHead)
            <> (map (toGargList ((isStopTerm s) . fst) MapTerm)       candidatesMap)
            <> (map (toGargList ((isStopTerm s) . fst) CandidateTerm) candidatesTailFinal)

    ngs = List.concat
        $ map toNgramsElement
        $ groupStems
        $ map (\(listType, (t,d)) -> ( ngramsGroup l n m t
                                     , GroupedText listType t d Set.empty
                                     )
              ) termList

  pure $ Map.fromList [(NgramsTerms, ngs)]

type Group = Lang -> Int -> Int -> Text -> Text
type Stem  = Text
type Label = Text
data GroupedText = GroupedText { _gt_listType :: ListType
                               , _gt_label    :: Label
                               , _gt_score    :: Double
                               , _gt_group    :: Set Text
                               }
groupStems :: [(Stem, GroupedText)] -> [GroupedText]
groupStems = Map.elems . Map.fromListWith grouping
  where
    grouping (GroupedText lt1 label1 score1 group1)
             (GroupedText lt2 label2 score2 group2)
             | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr)
             | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr)
        where
          lt = lt1 <> lt2
          gr = Set.union group1 group2

toNgramsElement :: GroupedText -> [NgramsElement]
toNgramsElement (GroupedText listType label _ setNgrams) =
  [parentElem] <> childrenElems
    where
      parent = label
      children = Set.toList setNgrams
      parentElem    = mkNgramsElement (NgramsTerm parent)
                                      listType
                                      Nothing
                                      (mSetFromList (NgramsTerm <$> children))
      childrenElems = map (\t -> mkNgramsElement t listType
                                                 (Just $ RootParent (NgramsTerm parent) (NgramsTerm parent))
                                                 (mSetFromList [])
                          ) (NgramsTerm <$> children)


toGargList :: (b -> Bool) -> ListType -> b -> (ListType, b)
toGargList isStop l n = case isStop n of
    True  -> (StopTerm, n)
    False -> (l, n)


isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
