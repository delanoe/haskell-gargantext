{-|
Module      : Gargantext.Core.Text.List.Social
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

module Gargantext.Core.Text.List.Social
  where

-- findList imports
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Prelude

-- filterList imports
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Schema.Ngrams
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set


flowSocialList :: ( RepoCmdM env err m
                        , CmdM     env err m
                        , HasNodeError err
                        , HasTreeError err
                        )
                     => User -> NgramsType -> Set Text
                     -> m (Map ListType (Set Text))
flowSocialList user nt ngrams' = do
  privateMapList <- flowSocialListByMode Private user nt ngrams'
  sharedMapList  <- flowSocialListByMode Shared  user nt (termsByList CandidateTerm privateMapList)
   -- TODO publicMapList

  pure $ Map.fromList [ (MapTerm,  termsByList MapTerm privateMapList
                                     <> termsByList MapTerm sharedMapList
                        )
                      , (StopTerm, termsByList StopTerm privateMapList
                                     <> termsByList StopTerm sharedMapList
                        )
                      , (CandidateTerm, termsByList CandidateTerm sharedMapList)
                      ]


termsByList :: ListType -> (Map (Maybe ListType) (Set Text)) -> Set Text
termsByList CandidateTerm m =
  fromMaybe Set.empty
          $ (<>) <$> Map.lookup Nothing              m
                 <*> Map.lookup (Just CandidateTerm) m
termsByList l m =
  fromMaybe Set.empty $ Map.lookup (Just l) m





flowSocialListByMode :: ( RepoCmdM env err m
                        , CmdM     env err m
                        , HasNodeError err
                        , HasTreeError err
                        )
                     => NodeMode -> User -> NgramsType -> Set Text
                     -> m (Map (Maybe ListType) (Set Text))
flowSocialListByMode mode user nt ngrams' = do
  listIds <- findListsId mode user
  counts  <- countFilterList ngrams' nt listIds Map.empty
  pure $ toSocialList counts ngrams'

---------------------------------------------------------------------------
-- TODO: maybe use social groups too
toSocialList :: Map Text (Map ListType Int)
             -> Set Text
             -> Map (Maybe ListType) (Set Text)
toSocialList m = Map.fromListWith (<>)
               . Set.toList
               . Set.map (toSocialList1 m)

-- | TODO what if equality ?
-- choice depends on Ord instance of ListType
-- for now : data ListType  =  StopTerm | CandidateTerm | MapTerm
-- means MapTerm > CandidateTerm > StopTerm in case of equality of counts
-- (we minimize errors on MapTerms if doubt)
toSocialList1 :: Map Text (Map ListType Int)
             -> Text
             -> (Maybe ListType, Set Text)
toSocialList1 m t = case Map.lookup t m of
  Nothing -> (Nothing, Set.singleton t)
  Just  m' -> ( (fst . fst) <$> Map.maxViewWithKey m'
              , Set.singleton t
              )

---------------------------------------------------------------------------
-- | [ListId] does not merge the lists (it is for Master and User lists
-- here we need UserList only
countFilterList :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList st nt ls input =
  foldM' (\m l -> countFilterList' st nt [l] m) input ls


countFilterList' :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList' st nt ls input = do
  ml <- toMapTextListType <$> getListNgrams ls nt
  pure $ Set.foldl' (\m t -> countList t ml m) input st

---------------------------------------------------------------------------
toMapTextListType :: Map Text NgramsRepoElement -> Map Text ListType
toMapTextListType = Map.fromListWith (<>)
              . List.concat
              . (map toList)
              . Map.toList

toList :: (Text, NgramsRepoElement) -> [(Text, ListType)]
toList (t, NgramsRepoElement _ lt root parent (MSet children)) =
     List.zip terms (List.cycle [lt])
      where
        terms =  [t]
              <> maybe [] (\n -> [unNgramsTerm n]) root
              <> maybe [] (\n -> [unNgramsTerm n]) parent
              <> (map unNgramsTerm $ Map.keys children)

---------------------------------------------------------------------------
countList :: Text
          -> Map Text ListType
          -> Map Text (Map ListType Int)
          -> Map Text (Map ListType Int)
countList t m input = case Map.lookup t m of
  Nothing -> input
  Just l  -> Map.alter addList t input
    where
      addList Nothing   = Just $ addCount l Map.empty
      addList (Just lm) = Just $ addCount l lm

addCount :: ListType -> Map ListType Int -> Map ListType Int
addCount l m = Map.alter plus l  m
  where
    plus Nothing  = Just 1
    plus (Just x) = Just $ x + 1

------------------------------------------------------------------------
findListsId :: (HasNodeError err, HasTreeError err)
            => NodeMode -> User -> Cmd err [NodeId]
findListsId mode u = do
  r <- getRootId u
  map _dt_nodeId <$> filter (\n -> _dt_typeId n == nodeTypeId NodeList)
                 <$> findNodes' mode r

findNodes' :: HasTreeError err
          => NodeMode -> RootId
          -> Cmd err [DbTreeNode]
findNodes' Private r = findNodes Private r [NodeFolderPrivate, NodeCorpus, NodeList]
findNodes' Shared  r = findNodes Shared  r [NodeFolderShared , NodeCorpus, NodeList]
findNodes' Public  r = findNodes Public  r [NodeFolderPublic , NodeCorpus, NodeList]
