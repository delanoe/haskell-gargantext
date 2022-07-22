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
import Gargantext.Core.Types (ListType(..), NodeId, NodeType(..), ListId)
import Gargantext.Database.Prelude (CmdM, HasConnectionPool(..))
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import Gargantext.Core.NodeStory
import qualified Gargantext.Core.NodeStoryFile as NSF


mergeNgramsElement :: NgramsRepoElement -> NgramsRepoElement -> NgramsRepoElement
mergeNgramsElement _neOld neNew = neNew

type RootTerm = NgramsTerm


getRepo :: HasNodeStory env err m
         => [ListId] -> m NodeListStory
getRepo listIds = do
  f <- getNodeListStory
  v  <- liftBase $ f listIds
  v' <- liftBase $ readMVar v
  pure $ v'


repoSize :: Ord k1 => NodeStory (Map.Map k1 (Map.Map k2 a)) p
                   -> NodeId
                   -> Map.Map k1 Int
repoSize repo node_id = Map.map Map.size state
  where
    state = repo ^. unNodeStory
                  . at node_id . _Just
                  . a_state


getNodeStoryVar :: HasNodeStory env err m
           => [ListId] -> m (MVar NodeListStory)
getNodeStoryVar l = do
  f <- getNodeListStory
  v  <- liftBase $ f l
  pure v


getNodeListStory :: HasNodeStory env err m
                 => m ([NodeId] -> IO (MVar NodeListStory))
getNodeListStory = do
  env <- view hasNodeStory
  pure $ view nse_getter env



listNgramsFromRepo :: [ListId]
                   -> NgramsType
                   -> NodeListStory
                   -> HashMap NgramsTerm NgramsRepoElement
listNgramsFromRepo nodeIds ngramsType repo =
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
getListNgrams :: HasNodeStory env err m
              => [ListId] -> NgramsType
              -> m (HashMap NgramsTerm NgramsRepoElement)
getListNgrams nodeIds ngramsType = listNgramsFromRepo nodeIds ngramsType
                                 <$> getRepo nodeIds


getTermsWith :: (HasNodeStory env err m, Eq a, Hashable a)
          => (NgramsTerm -> a) -> [ListId]
          -> NgramsType -> Set ListType
          -> m (HashMap a [a])
getTermsWith f ls ngt lts  = HM.fromListWith (<>)
                      <$> map toTreeWith
                      <$> HM.toList
                      <$> HM.filter (\f' -> Set.member (fst f') lts)
                      <$> mapTermListRoot ls ngt
                      <$> getRepo ls
  where
    toTreeWith (t, (_lt, maybeRoot)) = case maybeRoot of
      Nothing -> (f t, [])
      Just  r -> (f r, [f t])



mapTermListRoot :: [ListId]
                -> NgramsType
                -> NodeListStory
                -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
mapTermListRoot nodeIds ngramsType repo =
      (\nre -> (_nre_list nre, _nre_root nre))
  <$> listNgramsFromRepo nodeIds ngramsType repo




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

filterListWithRoot :: [ListType]
                   -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                   -> HashMap NgramsTerm (Maybe RootTerm)
filterListWithRoot lt m = snd <$> HM.filter isMapTerm m
  where
    isMapTerm (l, maybeRoot) = case maybeRoot of
      Nothing -> elem l lt
      Just  r -> case HM.lookup r m of
        Nothing -> panic $ "[Garg.API.Ngrams.Tools] filterWithRoot, unknown key: " <> unNgramsTerm r
        Just  (l',_) -> elem l' lt

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


migrateFromDirToDb :: (CmdM env err m, HasNodeStory env err m)
                   => NSF.NodeStoryDir ->  m ()
migrateFromDirToDb dir = do
  pool <- view connPool
  listIds <- liftBase $ getNodesIdWithType pool NodeList
  printDebug "[migrateFromDirToDb] listIds" listIds
  (NodeStory nls) <- NSF.getRepoNoEnv dir listIds
  printDebug "[migrateFromDirToDb] nls" nls
  _ <- mapM (\(nId, a) -> do
                n <- liftBase $ nodeExists pool nId
                case n of
                  False -> pure 0
                  True  -> liftBase $ upsertNodeArchive pool nId a
            ) $ Map.toList nls
  --_ <- nodeStoryIncs (Just $ NodeStory nls) listIds
  pure ()
