{-|
Module      : Gargantext.API.Ngrams.Tools
Description : Tools to manage Ngrams Elements (from the API)
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

getRepo :: RepoCmdM env err m => m NgramsRepo
getRepo = do
  v <- view repoVar
  liftBase $ readMVar v

listNgramsFromRepo :: [ListId] -> NgramsType
                   -> NgramsRepo -> Map Text NgramsRepoElement
listNgramsFromRepo nodeIds ngramsType repo = ngrams
  where
    ngramsMap = repo ^. r_state . at ngramsType . _Just

    ngrams    = Map.unionsWith mergeNgramsElement
              [ ngramsMap ^. at nodeId . _Just | nodeId <- nodeIds ]

-- TODO-ACCESS: We want to do the security check before entering here.
--              Add a static capability parameter would be nice.
--              Ideally this is the access to `repoVar` which needs to
--              be properly guarded.
getListNgrams :: RepoCmdM env err m
              => [ListId] -> NgramsType
              -> m (Map Text NgramsRepoElement)
getListNgrams nodeIds ngramsType = listNgramsFromRepo nodeIds ngramsType <$> getRepo

getTermsWith :: (RepoCmdM env err m, Ord a)
          => (Text -> a ) -> [ListId]
          -> NgramsType -> ListType
          -> m (Map a [a])
getTermsWith f ls ngt lt = Map.fromListWith (<>)
                      <$> map (toTreeWith f)
                      <$> Map.toList
                      <$> Map.filter (\f' -> (fst f') == lt)
                      <$> mapTermListRoot ls ngt
                      <$> getRepo
  where
    toTreeWith f'' (t, (_lt, maybeRoot)) = case maybeRoot of
      Nothing -> (f'' t, [])
      Just  r -> (f'' r, map f'' [t])

mapTermListRoot :: [ListId] -> NgramsType
                -> NgramsRepo -> Map Text (ListType, (Maybe Text))
mapTermListRoot nodeIds ngramsType repo =
  Map.fromList [ (t, (_nre_list nre, _nre_root nre))
               | (t, nre) <- Map.toList ngrams
               ]
  where ngrams = listNgramsFromRepo nodeIds ngramsType repo

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
getCoocByNgrams = getCoocByNgrams' identity


getCoocByNgrams' :: (Ord a, Ord c) => (b -> Set c) -> Diagonal -> Map a b -> Map (a, a) Int
getCoocByNgrams' f (Diagonal diag) m =
  Map.fromList [( (t1,t2)
                , maybe 0 Set.size $ Set.intersection
                                  <$> (fmap f $ Map.lookup t1 m)
                                  <*> (fmap f $ Map.lookup t2 m)
                ) | (t1,t2) <- case diag of
                                 True   -> [ (x,y) | x <- Map.keys m, y <- Map.keys m, x <= y]
                                 False  -> listToCombi identity (Map.keys m)
               ]

