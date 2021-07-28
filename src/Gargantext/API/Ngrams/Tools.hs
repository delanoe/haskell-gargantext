{-|
Module      : Gargantext.API.Ngrams.Tools
Description : Tools to manage Ngrams Elements (from the API)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeFamilies #-}

module Gargantext.API.Ngrams.Tools
  where

import Control.Concurrent
import Control.Lens (_Just, (^.), at, view, At, Index, IxValue)
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Validity
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType(..), NodeId, ListId)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.List           as List
import Gargantext.Core.NodeStory

mergeNgramsElement :: NgramsRepoElement -> NgramsRepoElement -> NgramsRepoElement
mergeNgramsElement _neOld neNew = neNew

type RootTerm = NgramsTerm

getRepo :: RepoCmdM env err m => m NgramsRepo
getRepo = do
  v <- view repoVar
  liftBase $ readMVar v

getRepo' :: HasNodeStory env err m
         => [ListId] -> m NodeListStory
getRepo' listIds = do
  maybeNodeListStory <- head <$> List.reverse <$> mapM getNodeListStory'' listIds
  case maybeNodeListStory of
    Nothing  -> panic "[G.A.N.Tools.getRepo']"
    Just nls -> pure nls 

getRepoVar :: HasNodeStory env err m
           => ListId -> m (MVar NodeListStory)
getRepoVar l = do
  f <- getNodeListStory
  v  <- liftBase $ f l
  pure v

getNodeListStory :: HasNodeStory env err m
                 => m (NodeId -> IO (MVar NodeListStory))
getNodeListStory = do
  env <- view hasNodeStory
  pure $ view nse_getter env

getNodeListStory' :: HasNodeStory env err m
                 => NodeId -> m (IO NodeListStory)
getNodeListStory' n = do
  f <- getNodeListStory
  v <- liftBase $ f n
  pure $ readMVar v

getNodeListStory'' :: HasNodeStory env err m
                 => NodeId -> m NodeListStory
getNodeListStory'' n = do
  f <- getNodeListStory
  v  <- liftBase $ f n
  v' <- liftBase $ readMVar v
  pure $ v'



listNgramsFromRepo :: [ListId] -> NgramsType
                   -> NgramsRepo -> HashMap NgramsTerm NgramsRepoElement
listNgramsFromRepo nodeIds ngramsType repo = ngrams
  where
    ngramsMap = repo ^. r_state . at ngramsType . _Just

    -- TODO HashMap linked
    ngrams    = HM.fromList $ Map.toList $ Map.unionsWith mergeNgramsElement
                [ ngramsMap ^. at nodeId . _Just | nodeId <- nodeIds ]


listNgramsFromRepo' :: [ListId] -> NgramsType
                   -> NodeListStory -> HashMap NgramsTerm NgramsRepoElement
listNgramsFromRepo' nodeIds ngramsType repo =
  HM.fromList $ Map.toList
              $ Map.unionsWith mergeNgramsElement ngrams
    where
      ngrams = [ repo
               ^. unNodeStory
                . at nodeId . _Just
                . a_state
                . at ngramsType . _Just
                | nodeId <- nodeIds
                ]



-- TODO-ACCESS: We want to do the security check before entering here.
--              Add a static capability parameter would be nice.
--              Ideally this is the access to `repoVar` which needs to
--              be properly guarded.
getListNgrams :: RepoCmdM env err m
              => [ListId] -> NgramsType
              -> m (HashMap NgramsTerm NgramsRepoElement)
getListNgrams nodeIds ngramsType = listNgramsFromRepo nodeIds ngramsType <$> getRepo

getListNgrams' :: HasNodeStory env err m
              => [ListId] -> NgramsType
              -> m (HashMap NgramsTerm NgramsRepoElement)
getListNgrams' nodeIds ngramsType = listNgramsFromRepo' nodeIds ngramsType
                                 <$> getRepo' nodeIds


getTermsWith :: (RepoCmdM env err m, Eq a, Hashable a)
          => (NgramsTerm -> a) -> [ListId]
          -> NgramsType -> Set ListType
          -> m (HashMap a [a])
getTermsWith f ls ngt lts = HM.fromListWith (<>)
                      <$> map toTreeWith
                      <$> HM.toList
                      <$> HM.filter (\f' -> Set.member (fst f') lts)
                      <$> mapTermListRoot ls ngt
                      <$> getRepo
  where
    toTreeWith (t, (_lt, maybeRoot)) = case maybeRoot of
      Nothing -> (f t, [])
      Just  r -> (f r, [f t])
getTermsWith' :: (HasNodeStory env err m, Eq a, Hashable a)
          => (NgramsTerm -> a) -> [ListId]
          -> NgramsType -> Set ListType
          -> m (HashMap a [a])
getTermsWith' f ls ngt lts  = HM.fromListWith (<>)
                      <$> map toTreeWith
                      <$> HM.toList
                      <$> HM.filter (\f' -> Set.member (fst f') lts)
                      <$> mapTermListRoot' ls ngt
                      <$> getRepo' ls
  where
    toTreeWith (t, (_lt, maybeRoot)) = case maybeRoot of
      Nothing -> (f t, [])
      Just  r -> (f r, [f t])




mapTermListRoot :: [ListId]
                -> NgramsType
                -> NgramsRepo
                -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
mapTermListRoot nodeIds ngramsType repo =
      (\nre -> (_nre_list nre, _nre_root nre))
  <$> listNgramsFromRepo nodeIds ngramsType repo
mapTermListRoot' :: [ListId]
                -> NgramsType
                -> NodeListStory
                -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
mapTermListRoot' nodeIds ngramsType repo =
      (\nre -> (_nre_list nre, _nre_root nre))
  <$> listNgramsFromRepo' nodeIds ngramsType repo





filterListWithRootHashMap :: ListType
                          -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                          -> HashMap NgramsTerm (Maybe RootTerm)
filterListWithRootHashMap lt m = snd <$> HM.filter isMapTerm m
  where
    isMapTerm (l, maybeRoot) = case maybeRoot of
      Nothing -> l == lt
      Just  r -> case HM.lookup r m of
        Nothing -> panic $ "[Garg.API.Ngrams.Tools] filterWithRoot, unknown key: " <> unNgramsTerm r
        Just  (l',_) -> l' == lt

filterListWithRoot :: ListType
                   -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                   -> HashMap NgramsTerm (Maybe RootTerm)
filterListWithRoot lt m = snd <$> HM.filter isMapTerm m
  where
    isMapTerm (l, maybeRoot) = case maybeRoot of
      Nothing -> l == lt
      Just  r -> case HM.lookup r m of
        Nothing -> panic $ "[Garg.API.Ngrams.Tools] filterWithRoot, unknown key: " <> unNgramsTerm r
        Just  (l',_) -> l' == lt

groupNodesByNgrams :: ( At root_map
                      , Index root_map ~ NgramsTerm
                      , IxValue root_map ~ Maybe RootTerm
                      )
                   => root_map
                   -> HashMap NgramsTerm (Set NodeId)
                   -> HashMap NgramsTerm (Set NodeId)
groupNodesByNgrams syn occs = HM.fromListWith (<>) occs'
  where
    occs' = map toSyn (HM.toList occs)
    toSyn (t,ns) = case syn ^. at t of
      Nothing -> panic $ "[Garg.API.Ngrams.Tools.groupNodesByNgrams] unknown key: " <> unNgramsTerm t
      Just  r -> case r of
        Nothing  -> (t, ns)
        Just  r' -> (r',ns)

data Diagonal = Diagonal Bool

getCoocByNgrams :: Diagonal
                -> HashMap NgramsTerm (Set NodeId)
                -> HashMap (NgramsTerm, NgramsTerm) Int
getCoocByNgrams = getCoocByNgrams' identity


getCoocByNgrams' :: (Hashable a, Ord a, Ord c)
                 => (b -> Set c)
                 -> Diagonal
                 -> HashMap a b
                 -> HashMap (a, a) Int
getCoocByNgrams' f (Diagonal diag) m =
  HM.fromList [( (t1,t2)
               , maybe 0 Set.size $ Set.intersection
                                 <$> (fmap f $ HM.lookup t1 m)
                                 <*> (fmap f $ HM.lookup t2 m)
               )
              | (t1,t2) <- if diag then
                             [ (x,y) | x <- ks, y <- ks, x <= y] -- TODO if we keep a Data.Map here it might be
                                                                 -- more efficient to enumerate all the y <= x.
                           else
                             listToCombi identity ks
              ]

  where ks = HM.keys m

------------------------------------------
