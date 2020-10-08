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
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Admin.Config

-- filterList imports
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Data.Map (Map)
import Data.Tuple (fst)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Gargantext.API.Ngrams.Types
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams
import Gargantext.Core.Types.Main
import Gargantext.Database.Schema.Ngrams


---------------------------------------------------------------------------
-- TODO: maybe use social groups too
toSocialList :: Map Text (Map ListType Int)
             -> Set Text
             -> Set (Text, Maybe ListType)
toSocialList m = Set.map (toSocialList1 m)

-- | TODO what if equality ?
-- choice depends on Ord instance of ListType
-- for now : data ListType  =  StopTerm | CandidateTerm | MapTerm
-- means MapTerm > CandidateTerm > StopTerm in case of equality of counts
-- (we minimize errors on MapTerms if doubt)
toSocialList1 :: Map Text (Map ListType Int)
             -> Text
             -> (Text, Maybe ListType)
toSocialList1 m t = case Map.lookup t m of
  Nothing -> (t, Nothing)
  Just  m -> (t, (fst . fst) <$> Map.maxViewWithKey m)

---------------------------------------------------------------------------
-- | [ListId] does not merge the lists (it is for Master and User lists
-- here we need UserList only
countFilterList :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList st nt ls input = foldM' (\m l -> countFilterList' st nt [l] m) input ls


countFilterList' :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList' st nt ls input = do
  ml <- toMapListType <$> getListNgrams ls nt
  pure $ Set.foldl' (\m t -> countList t ml m) input st

---------------------------------------------------------------------------
toMapListType :: Map Text NgramsRepoElement -> Map Text ListType
toMapListType = Map.fromListWith (<>)
              . List.concat
              . (map toList)
              . Map.toList

toList :: (Text, NgramsRepoElement) -> [(Text, ListType)]
toList (t, NgramsRepoElement _ lt r parent (MSet children)) =
     List.zip terms (List.cycle [lt])
      where
        terms = [t]
              <> maybe [] (\n -> [unNgramsTerm n]) parent
              <> (map unNgramsTerm $ Map.keys children)

---------------------------------------------------------------------------

countList :: Text
          -> Map Text ListType
          -> Map Text (Map ListType Int)
          -> Map Text (Map ListType Int)
countList t m input = case Map.lookup t m of
  Nothing -> input
  Just l  -> Map.alter add t input
    where
      add Nothing   = Just $ addCount l Map.empty
      add (Just lm) = Just $ addCount l lm

addCount :: ListType -> Map ListType Int -> Map ListType Int
addCount l m = Map.alter add l  m
  where
    add Nothing  = Just 1
    add (Just x) = Just $ x + 1

------------------------------------------------------------------------

findListsId :: (HasNodeError err, HasTreeError err) => NodeMode -> User -> Cmd err [NodeId]
findListsId mode u = do
  r <- getRootId u
  map _dt_nodeId <$> filter (\n -> _dt_typeId n == nodeTypeId NodeList)
                 <$> findNodes' mode r

findNodes' :: HasTreeError err
          => NodeMode -> RootId
          -> Cmd err [DbTreeNode]
findNodes' Private r = findNodes Private r [NodeFolderPrivate, NodeCorpus, NodeList]
findNodes' Shared  r = findNodes Shared  r [NodeFolderShared, NodeCorpus, NodeList]
findNodes' Public  r = findNodes Public  r [NodeFolderPublic, NodeCorpus, NodeList]
