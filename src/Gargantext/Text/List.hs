{-|
Module      : Gargantext.Text.Ngrams.Lists
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Gargantext.Text.List
  where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, RootParent(..), mSetFromList)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Database.Metrics.NgramsByNode (getTficf', sortTficf, ngramsGroup, getNodesByNgramsUser, groupNodesByNgramsWith)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Char as Char

-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: UserCorpusId -> MasterCorpusId
                 -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsLists uCid mCid = do
  ngTerms     <- buildNgramsTermsList uCid mCid
  othersTerms <- mapM (buildNgramsOthersList uCid identity) [Authors, Sources, Institutes]
  pure $ Map.unions $ othersTerms <> [ngTerms]


buildNgramsOthersList :: UserCorpusId -> (Text -> Text) -> NgramsType 
                      -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsOthersList uCid groupIt nt = do
  ngs <- groupNodesByNgramsWith groupIt <$> getNodesByNgramsUser uCid nt

  pure $ Map.fromList [(nt, [ mkNgramsElement t CandidateTerm Nothing (mSetFromList [])
                            | (t,_ns) <- Map.toList ngs
                            ]
                        )
                      ]



buildNgramsTermsList :: UserCorpusId -> MasterCorpusId
                -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsTermsList uCid mCid = do
            candidates   <- sortTficf <$> getTficf' uCid mCid (ngramsGroup EN 2)
            --printDebug "candidate" (length candidates)

            let termList = toTermList (isStopTerm . fst) candidates
            --printDebug "termlist" (length termList)

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

      a = 100
      b = 1000

isStopTerm :: Text -> Bool
isStopTerm x = Text.length x < 3
             || not (all Char.isAlpha (Text.unpack x'))
                where
                  x' = ( Text.replace "-" ""
                       . Text.replace " " ""
                       . Text.replace "/" ""
                       ) x


