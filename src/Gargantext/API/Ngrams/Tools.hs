{-|
Module      : Gargantext.API.Ngrams.Tools
Description : Tools to manage Ngrams Elements (from the API)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.API.Ngrams.Tools
  where

import Control.Concurrent
import Control.Lens (_Just, (^.), at, view)
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validity
import Gargantext.API.Ngrams
import Gargantext.Core.Types (ListType(..), NodeId, ListId)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type RootTerm = Text


getListNgrams :: RepoCmdM env err m
               => [ListId] -> NgramsType
               -> m (Map Text NgramsRepoElement)
getListNgrams nodeIds ngramsType = do
  v    <- view repoVar
  repo <- liftIO $ readMVar v

  let
    ngramsMap = repo ^. r_state . at ngramsType . _Just

    ngrams    = Map.unionsWith mergeNgramsElement
              [ ngramsMap ^. at nodeId . _Just | nodeId <- nodeIds ]

  pure ngrams

mapTermListRoot :: RepoCmdM env err m
               => [ListId] -> NgramsType
               -> m (Map Text (ListType, (Maybe Text)))
mapTermListRoot nodeIds ngramsType = do
  ngrams <- getListNgrams nodeIds ngramsType
  pure $ Map.fromList [(t, (_nre_list nre, _nre_root nre))
                      | (t, nre) <- Map.toList ngrams
                      ]


filterListWithRoot :: ListType -> Map Text (ListType, Maybe Text)
                      -> Map Text (Maybe RootTerm)
filterListWithRoot lt m = Map.fromList
                    $ map (\(t,(_,r)) -> (t,r))
                    $ filter isGraphTerm (Map.toList m)
  where
    isGraphTerm (_t,(l, maybeRoot)) = case maybeRoot of
      Nothing -> l == lt
      Just  r -> case Map.lookup r m of
        Nothing -> panic $ "Garg.API.Ngrams.Tools: filterWithRoot, unknown key: " <> r
        Just  (l',_) -> l' == lt


groupNodesByNgrams :: Map Text (Maybe RootTerm)
                   -> Map Text (Set NodeId)
                   -> Map Text (Set NodeId)
groupNodesByNgrams syn occs = Map.fromListWith (<>) occs'
  where
    occs' = map toSyn (Map.toList occs)
    toSyn (t,ns) = case Map.lookup t syn of
      Nothing -> panic $ "[Garg.API.Ngrams.Tools.groupNodesByNgrams] unknown key: " <> t
      Just  r -> case r of
        Nothing  -> (t, ns)
        Just  r' -> (r',ns)

data Diagonal = Diagonal Bool

getCoocByNgrams :: Diagonal -> Map Text (Set NodeId) -> Map (Text, Text) Int
getCoocByNgrams (Diagonal diag) m =
  Map.fromList [((t1,t2)
                ,maybe 0 Set.size $ Set.intersection
                                 <$> Map.lookup t1 m
                                 <*> Map.lookup t2 m
                ) | (t1,t2) <- case diag of
                                 True   -> [ (x,y) | x <- Map.keys m, y <- Map.keys m, x <= y]
                                 False  -> listToCombi identity (Map.keys m)
               ]


