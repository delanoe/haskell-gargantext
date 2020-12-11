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

import Control.Lens hiding (cons)
import Data.Map (Map)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types (ListId)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map.Strict as Map

-- TODO put this in Prelude maybe
cons :: a -> [a]
cons a = [a]

------------------------------------------------------------------------
-- | History control
data History = User
             | NotUser
             | AllHistory

------------------------------------------------------------------------
-- | Main Function
history :: History
        -> [NgramsType]
        -> [ListId]
        -> Repo s NgramsStatePatch
        -> Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])
history User t l = clean . (history' t l)
  where
    clean = Map.map (Map.map List.init)

history NotUser t l = clean . (history' t l)
  where
    clean = Map.map (Map.map last)
    last = (maybe [] cons) . lastMay

history AllHistory t l = history' t l

------------------------------------------------------------------------

history' :: [NgramsType]
        -> [ListId]
        -> Repo s NgramsStatePatch
        -> Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])
history' types lists = merge
                    . map (Map.map ( Map.map cons))
                    . map (Map.map ((Map.filterWithKey (\k _ -> List.elem k lists))))
                    . map           (Map.filterWithKey (\k _ -> List.elem k types))
                    . map toMap
                    . view r_history


merge :: [Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])]
      ->  Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])
merge = Map.unionsWith merge'
  where
    merge' :: Map ListId [Map NgramsTerm NgramsPatch]
           -> Map ListId [Map NgramsTerm NgramsPatch]
           -> Map ListId [Map NgramsTerm NgramsPatch]
    merge' = Map.unionWith (<>)


toMap :: PatchMap NgramsType
           (PatchMap ListId
            (NgramsTablePatch
            )
          )
        -> Map NgramsType
           (Map ListId
            (Map NgramsTerm NgramsPatch
            )
           )
toMap = Map.map (Map.map unNgramsTablePatch) . (Map.map unPatchMap) . unPatchMap


