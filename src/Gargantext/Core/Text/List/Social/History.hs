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
                 . map toMap
                 . view r_history
  where
    cons a = a : []

toMap :: PatchMap NgramsType
           (PatchMap NodeId
            (NgramsTablePatch
            )
          )
        -> Map NgramsType
           (Map ListId
            (Map NgramsTerm NgramsPatch
            )
           )
toMap = Map.map (Map.map unNgramsTablePatch) . (Map.map toMap') . toMap'
  where
    toMap' :: Ord a => PatchMap a b -> Map a b
    toMap' = Map.fromList . PatchMap.toList

    unNgramsTablePatch :: NgramsTablePatch -> Map NgramsTerm NgramsPatch
    unNgramsTablePatch (NgramsTablePatch p) = toMap' p

