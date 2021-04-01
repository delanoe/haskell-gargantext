{-|
Module      : Gargantext.API.Ngrams.Prelude
Description : Tools to manage Ngrams Elements (from the API)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeFamilies #-}

module Gargantext.API.Ngrams.Prelude
  where

import Data.Maybe (catMaybes)
import Control.Lens (view)
import Data.Hashable (Hashable)
import Data.Validity
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.Context (TermList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Text as Text

------------------------------------------------------------------------
-- | Tools
-- Usage example: toTermList MapTerm NgramsTerms ngramsList
toTermList :: ListType -> NgramsType -> NgramsList -> Maybe TermList
toTermList lt nt nl = toTermList' lt <$> Map.lookup nt nl
  where
    toTermList' :: ListType -> Versioned NgramsTableMap -> TermList
    toTermList' lt' = (toTermList'' lt') . Map.toList . view v_data

    toTermList'' :: ListType -> [(NgramsTerm, NgramsRepoElement)] -> TermList
    toTermList'' lt'' ns = Map.toList
                       $ Map.mapKeys toTerm
                       $ Map.fromListWith (<>) (roots' <> children')
      where
        toTerm = Text.splitOn " " . unNgramsTerm

        (roots, children) = List.partition (\(_t, nre) -> view nre_root nre == Nothing)
                          $ List.filter (\(_t,nre) -> view nre_list nre == lt'') ns

        roots'    = catMaybes
                   $ map (\(t,nre) -> (,) <$> Just t
                                           <*> Just (map toTerm $ unMSet 
                                                                $ view nre_children nre
                                                    )
                         ) roots

        children' = catMaybes
                  $ map (\(t,nre) -> (,) <$> view nre_root nre
                                          <*> Just (map toTerm $ [t] 
                                                              <> (unMSet $ view nre_children nre)
                                                   )
                        ) children 

------------------------------------------
patchMSet_toList :: (Ord a, Hashable a) => PatchMSet a -> [(a,AddRem)]
patchMSet_toList = HM.toList . unPatchMapToHashMap . unPatchMSet

unMSet :: MSet a -> [a]
unMSet (MSet a) = Map.keys a

