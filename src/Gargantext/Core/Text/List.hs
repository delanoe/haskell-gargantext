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

{-
buildNgramsTermsList' :: UserCorpusId
                      -> (Text -> Text)
                      -> ((Text, (Set Text, Set NodeId)) -> Bool)
                      -> Int
                      -> Int
                      -> Cmd err (Map NgramsType [NgramsElement])

buildNgramsTermsList' uCid groupIt stop gls is = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid NgramsTerms
  
  let
    (stops, candidates) = partitionEithers
                          $ map (\t -> if stop t then Left t else Right t)
                          $ Map.toList
                          $ Map.filter ((\s' -> Set.size s' > 1) . snd) ngs

    (maps, candidates') = takeScored gls is
                        $ getCoocByNgrams' snd (Diagonal True)
                        $ Map.fromList candidates


    toList' t = (fst t, (fromIntegral $ Set.size $ snd $ snd t, fst $ snd t))

    (s,c,m) = (stops
       , List.filter (\(k,_) -> List.elem k candidates') candidates
       , List.filter (\(k,_) -> List.elem k maps) candidates
       )

  let ngs' = List.concat
          $ map toNgramsElement
          $ map (\t -> (StopTerm     , toList' t)) s
         <> map (\t -> (CandidateTerm, toList' t)) c
         <> map (\t -> (MapTerm    , toList' t)) m

  pure $ Map.fromList [(NgramsTerms, ngs')]
-}




buildNgramsTermsList :: Lang
                     -> Int
                     -> Int
                     -> StopSize
                     -> UserCorpusId
                     -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList _l _n _m s uCid mCid = do
  candidates <- sortTficf Down <$> getTficf uCid mCid NgramsTerms

  let
    candidatesSize = 400
{-
    a = 50
    b = 50
-}
    candidatesHead = List.take candidatesSize candidates
    candidatesTail = List.drop candidatesSize candidates

    termList =
          -- (toTermList a b ((isStopTerm s) . fst) candidatesHead)
                (map (toGargList ((isStopTerm s) .fst) MapTerm)       candidatesHead)
             <> (map (toGargList ((isStopTerm s) .fst) CandidateTerm) candidatesTail)

    ngs = List.concat $ map toNgramsElement $ map (\(lt, (t,d)) -> (lt, ((t, (d,Set.singleton t))))) termList

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
toGargList stop l n = case stop n of
    True  -> (StopTerm, n)
    False -> (l, n)


isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
