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
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types (ListId)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map

-- TODO put this in Prelude
cons :: a -> [a]
cons a = [a]

------------------------------------------------------------------------
-- | History control
data History = History_User
             | History_NotUser
             | History_All

------------------------------------------------------------------------
-- | Main Function
history :: History
        -> [NgramsType]
        -> [ListId]
        -> NodeStory s NgramsStatePatch'
        -> Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch])
history History_User t l = clean . (history' t l)
  where
    clean = Map.map (Map.map List.init)

history History_NotUser t l = clean . (history' t l)
  where
    clean = Map.map (Map.map last)
    last = (maybe [] cons) . lastMay

history _ t l = history' t l

------------------------------------------------------------------------
history' :: [NgramsType]
        -> [ListId]
        -> NodeStory s NgramsStatePatch'
        -> Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch])
history' types lists = (Map.map (Map.unionsWith (<>)))
                    . (Map.map (map (Map.filterWithKey (\k _ -> List.elem k types))))
                    . (Map.map (map toMap))
                    . (Map.map (view a_history))
                    . (Map.filterWithKey (\k _ -> List.elem k lists))
                    . (view unNodeStory)
  where

    toMap :: PatchMap NgramsType NgramsTablePatch
          -> Map NgramsType [HashMap NgramsTerm NgramsPatch]
    toMap m = Map.map (cons . unNgramsTablePatch)
            $ unPatchMapToMap m


