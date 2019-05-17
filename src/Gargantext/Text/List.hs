{-|
Module      : Gargantext.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Text.List
  where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, RootParent(..), mSetFromList)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Database.Metrics.NgramsByNode (getTficf', sortTficf, ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Text.List.Learn (Model(..))
import Gargantext.Prelude
--import Gargantext.Text.Terms (TermType(..))
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
    all' = Map.toList ngs
  pure $ (toElements GraphTerm $ take 10 all') <> (toElements CandidateTerm $ drop 10 all')
    where
      toElements nType x = Map.fromList [(nt, [ mkNgramsElement t nType Nothing (mSetFromList [])
                            | (t,_ns) <- x
                            ]
                        )
                      ]

buildNgramsTermsList :: Lang -> Int -> Int -> StopSize -> UserCorpusId -> MasterCorpusId
                     -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList l n m s uCid mCid = do
  candidates   <- sortTficf <$> getTficf' uCid mCid NgramsTerms (ngramsGroup l n m)
  let termList = toTermList ((isStopTerm s) . fst) candidates
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

-- TODO remove hard coded parameters
toTermList :: (a -> Bool) -> [a] -> [(ListType, a)]
toTermList stop ns =  map (toTermList' stop CandidateTerm) xs
                   <> map (toTermList' stop GraphTerm)     ys
                   <> map (toTermList' stop CandidateTerm) zs
    where
      toTermList' stop' l n = case stop' n of
          True  -> (StopTerm, n)
          False -> (l, n)

      -- TODO use % of size of list
      -- TODO user ML
      xs = take a ns
      ys = take b $ drop a ns
      zs = drop b $ drop a ns

      a = 3
      b = 500

isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
