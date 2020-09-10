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
import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, RootParent(..), mSetFromList)
-- import Gargantext.API.Ngrams.Tools (getCoocByNgrams', Diagonal(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId, Ordering(..))
import Gargantext.Database.Action.Metrics.NgramsByNode ({-ngramsGroup,-} getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Action.Metrics.TFICF (getTficf)
import Gargantext.Core.Text.Metrics.TFICF (sortTficf)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import Gargantext.Core.Text.List.Learn (Model(..))
-- import Gargantext.Core.Text.Metrics (takeScored)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text


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
        Map.fromList [(nt, [ mkNgramsElement t nType Nothing (mSetFromList [])
                           | (t,_ns) <- x
                           ]
                     )]

buildNgramsTermsList :: Lang
                     -> Int
                     -> Int
                     -> StopSize
                     -> UserCorpusId
                     -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList _l _n _m s uCid mCid = do
  candidates <- sortTficf Down <$> getTficf uCid mCid NgramsTerms
  printDebug "head candidates" (List.take 10 $ candidates)
  printDebug "tail candidates" (List.take 10 $ List.reverse $ candidates)

  let
    candidatesSize = 400
    candidatesHead = List.take candidatesSize candidates
    candidatesTail = List.drop candidatesSize candidates

    termList = (map (toGargList ((isStopTerm s) .fst) MapTerm)       candidatesHead)
            <> (map (toGargList ((isStopTerm s) .fst) CandidateTerm) candidatesTail)

    ngs = List.concat
        $ map toNgramsElement
        $ map (\(lt, (t,d)) -> (lt, ((t, (d,Set.singleton t))))) termList

  pure $ Map.fromList [(NgramsTerms, ngs)]


toTermList :: Int
           -> Int
           -> (a -> Bool)
           -> [a]
           -> [(ListType, a)]
toTermList _ _ _ [] = []
toTermList a b stop ns =  -- trace ("computing toTermList") $
                      map (toGargList stop CandidateTerm) xs
                   <> map (toGargList stop MapTerm)     ys
                   <> toTermList a b stop zs
    where
      xs = take a ns
      xz = drop a ns

      ys = take b xz
      zs = drop b xz


toNgramsElement :: (ListType, (Text, (Double, Set Text))) -> [NgramsElement]
toNgramsElement (listType, (_stem, (_score, setNgrams))) =
  case Set.toList setNgrams of
    []                -> []
    (parent:children) -> [parentElem] <> childrenElems
      where
        parentElem    = mkNgramsElement parent
                                        listType
                                        Nothing
                                        (mSetFromList children)
        childrenElems = map (\t -> mkNgramsElement t listType
                                                   (Just $ RootParent parent parent)
                                                   (mSetFromList [])
                            ) children


toGargList :: (b -> Bool) -> ListType -> b -> (ListType, b)
toGargList isStop l n = case isStop n of
    True  -> (StopTerm, n)
    False -> (l, n)


isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
