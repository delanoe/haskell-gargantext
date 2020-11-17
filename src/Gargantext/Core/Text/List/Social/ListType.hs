{-|
Module      : Gargantext.Core.Text.List.Social.ListType
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.ListType
  where

import Gargantext.Database.Admin.Types.Node
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Schema.Ngrams
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set

------------------------------------------------------------------------
-- | [ListId] does not merge the lists (it is for Master and User lists
-- here we need UserList only
countFilterList :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        ->    Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList st nt ls input =
  foldM' (\m l -> countFilterList' st nt [l] m) input ls
    where
      countFilterList' :: RepoCmdM env err m
              => Set Text -> NgramsType -> [ListId]
              ->    Map Text (Map ListType Int)
              -> m (Map Text (Map ListType Int))
      countFilterList' st' nt' ls' input' = do
        ml <- toMapTextListType <$> getListNgrams ls' nt'
        pure $ Set.foldl' (\m t -> countList t ml m) input' st'

------------------------------------------------------------------------
-- FIXME children have to herit the ListType of the parent
toMapTextListType :: Map Text NgramsRepoElement -> Map Text ListType
toMapTextListType m = Map.fromListWith (<>)
                    $ List.concat
                    $ map (toList m)
                    $ Map.toList m
  where
    toList :: Map Text NgramsRepoElement -> (Text, NgramsRepoElement) -> [(Text, ListType)]
    toList m' (t, nre@(NgramsRepoElement _ _ _ _ (MSet children))) =
         List.zip terms (List.cycle [lt'])
          where
            terms =  [t]
                  -- <> maybe [] (\n -> [unNgramsTerm n]) root
                  -- <> maybe [] (\n -> [unNgramsTerm n]) parent
                  <> (map unNgramsTerm $ Map.keys children)
            lt'   = listOf m' nre

            listOf :: Map Text NgramsRepoElement -> NgramsRepoElement -> ListType
            listOf m'' ng = case _nre_parent ng of
              Nothing -> _nre_list ng
              Just  p -> case Map.lookup (unNgramsTerm p) m'' of
                Just ng' -> listOf m'' ng'
                Nothing  -> CandidateTerm
                -- panic "[G.C.T.L.Social.listOf] Nothing: Should Not happen"

------------------------------------------------------------------------
countList :: Text
          -> Map Text ListType
          -> Map Text (Map ListType Int)
          -> Map Text (Map ListType Int)
countList t m input = case Map.lookup t m of
  Nothing -> input
  Just l  -> Map.alter addList t input
    where

      addList Nothing   = Just $ addCountList l Map.empty
      addList (Just lm) = Just $ addCountList l lm

      addCountList :: ListType -> Map ListType Int -> Map ListType Int
      addCountList l' m' = Map.alter (plus l') l'  m'
        where
          plus CandidateTerm Nothing  = Just 1
          plus CandidateTerm (Just x) = Just $ x + 1

          plus MapTerm Nothing              = Just 2
          plus MapTerm (Just x)             = Just $ x + 2

          plus StopTerm Nothing              = Just 3
          plus StopTerm (Just x)             = Just $ x + 3

