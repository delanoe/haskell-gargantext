{-|
Module      : Gargantext.Core.Text.List.Social.History
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.History
  where

import Data.Maybe (catMaybes)
import Data.Map (Map)
import Control.Lens (view)
import Gargantext.API.Ngrams.Types
import Gargantext.Prelude
import Gargantext.Core.Types (ListType(..), ListId, NodeId)
import qualified Data.Map.Strict.Patch as PatchMap
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Gargantext.Database.Schema.Ngrams (NgramsType(..))

toMap :: Ord a => PatchMap a b -> Map a b
toMap = Map.fromList . PatchMap.toList

toMap' :: (Ord a, Ord b) => PatchMap a (PatchMap b c) -> Map a (Map b c)
toMap' = (Map.map toMap) . toMap

-- type NgramsRepo       = Repo NgramsState NgramsStatePatch
-- type NgramsState      = Map      TableNgrams.NgramsType (Map NodeId NgramsTableMap)
-- type NgramsStatePatch = PatchMap TableNgrams.NgramsType (PatchMap NodeId NgramsTablePatch)
-- type NgramsTablePatch = Map NgramsTerm NgramsPatch


toMap'' :: NgramsStatePatch
        -> Map NgramsType
           (Map ListId
            (Map NgramsTerm NgramsPatch
            )
           )
toMap'' = undefined


history :: Foldable t
        => NgramsType
        -> t ListId
        -> Repo s NgramsStatePatch
        -> Map ListId [Map NgramsTerm NgramsPatch]
history nt lists = Map.unionsWith (<>)
                 . map (Map.map cons)
                 . map (Map.filterWithKey (\k _ -> List.elem k lists))
                 . catMaybes
                 . map (Map.lookup nt)
                 . map toMap''
                 . view r_history
  where
    cons a = a : []
