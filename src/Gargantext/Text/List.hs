{-|
Module      : Gargantext.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Text.List
  where

import Data.Either (partitionEithers, Either(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, RootParent(..), mSetFromList)
import Gargantext.API.Ngrams.Tools (getCoocByNgrams', Diagonal(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId, NodeId)
import Gargantext.Database.Action.Metrics.NgramsByNode (getTficf', sortTficf, ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Admin.Utils (Cmd)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import Gargantext.Text.List.Learn (Model(..))
import Gargantext.Text.Metrics (takeScored)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text


data NgramsListBuilder = BuilderStepO { stemSize :: Int
                                      , stemX    :: Int
                                      , stopSize :: Int
                                      }
                       | BuilderStep1 { withModel :: Model }
                       | BuilderStepN { withModel :: Model }
                       | Tficf { nlb_lang :: Lang
                               , nlb_group1 :: Int
                               , nlb_group2 :: Int
                               , nlb_stopSize :: StopSize
                               , nlb_userCorpusId :: UserCorpusId
                               , nlb_masterCorpusId :: MasterCorpusId
                               }


data StopSize = StopSize {unStopSize :: Int}

-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: Lang -> Int -> Int -> StopSize -> UserCorpusId -> MasterCorpusId
                 -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsLists l n m s uCid mCid = do
  ngTerms     <- buildNgramsTermsList l n m s uCid mCid
  othersTerms <- mapM (buildNgramsOthersList uCid identity) [Authors, Sources, Institutes]
  pure $ Map.unions $ othersTerms <> [ngTerms]


buildNgramsOthersList :: UserCorpusId -> (Text -> Text) -> NgramsType
                      -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsOthersList uCid groupIt nt = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt

  let
    listSize = 9
    all' = List.reverse $ List.sortOn (Set.size . snd . snd) $ Map.toList ngs
    graphTerms = List.take listSize all'
    candiTerms = List.drop listSize all'
  pure $ Map.unionsWith (<>) [ toElements GraphTerm     graphTerms
                             , toElements CandidateTerm candiTerms]
    where
      toElements nType x = Map.fromList [(nt, [ mkNgramsElement t nType Nothing (mSetFromList [])
                            | (t,_ns) <- x
                            ]
                        )
                      ]

--{-
buildNgramsTermsList' :: UserCorpusId
                      -> (Text -> Text)
                      -> ((Text, (Set Text, Set NodeId)) -> Bool) -> Int -> Int
                      -> Cmd err (Map NgramsType [NgramsElement])
--}
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
          $ map (\t -> (StopTerm, toList' t)) s
         <> map (\t -> (CandidateTerm, toList' t)) c
         <> map (\t -> (GraphTerm, toList' t)) m

  pure $ Map.fromList [(NgramsTerms, ngs')]


buildNgramsTermsList :: Lang -> Int -> Int -> StopSize -> UserCorpusId -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList l n m s uCid mCid = do
  candidates   <- sortTficf <$> getTficf' uCid mCid NgramsTerms (ngramsGroup l n m)
  let
    candidatesSize = 2000
    a = 10
    b = 10
    candidatesHead = List.take candidatesSize candidates
    candidatesTail = List.drop candidatesSize candidates
    termList = (toTermList a b ((isStopTerm s) . fst) candidatesHead)
             <> (map (toList ((isStopTerm s) .fst) CandidateTerm) candidatesTail)
  let ngs = List.concat $ map toNgramsElement termList

  pure $ Map.fromList [(NgramsTerms, ngs)]


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


toList :: (b -> Bool) -> ListType -> b -> (ListType, b)
toList stop l n = case stop n of
    True  -> (StopTerm, n)
    False -> (l, n)


toTermList :: Int -> Int -> (a -> Bool) -> [a] -> [(ListType, a)]
toTermList _ _ _ [] = []
toTermList a b stop ns =  -- trace ("computing toTermList") $
                      map (toList stop CandidateTerm) xs
                   <> map (toList stop GraphTerm)     ys
                   <> toTermList a b stop zs
    where
      xs = take a ns
      ta = drop a ns
      
      ys = take b ta
      zs = drop b ta


isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
