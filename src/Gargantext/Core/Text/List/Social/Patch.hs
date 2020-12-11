{-|
Module      : Gargantext.Core.Text.List.Social.Patch
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.Patch
  where

import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid
import Control.Lens hiding (cons)
import Gargantext.API.Ngrams.Types
import Gargantext.Prelude
import Gargantext.Core.Types (ListType(..), ListId, NodeId)
import Gargantext.Core.Text.List.Social
import Gargantext.Core.Text.List.Social.Prelude
import qualified Data.Map.Strict.Patch as PatchMap
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.API.Ngrams.Types
import qualified Data.Patch.Class as Patch (Replace(..))

{-
fromList [(NgramsTerms,fromList [(NodeId 189,
         
fromList [(NgramsTerm {unNgramsTerm = "journal"},NgramsReplace {_patch_old = Nothing, _patch_new = Just (NgramsRepoElement {_nre_size = 1, _nre_list = CandidateTerm, _nre_root = Nothing, _nre_parent = Nothing, _nre_children = MSet (fromList [])})})],f

[fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Just (), _new = Nothing})])), _patch_list = Keep})]
,fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Nothing, _new = Just ()})])), _patch_list = Keep})]

,fromList [(NgramsTerm {unNgramsTerm = "problem"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = CandidateTerm}})]
,fromList [(NgramsTerm {unNgramsTerm = "paper"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = StopTerm}})]])])]
-}

addScorePatch :: (NgramsTerm , NgramsPatch)
              -> FlowCont Text FlowListScores
              -> FlowCont Text FlowListScores
addScorePatch (NgramsTerm t, (NgramsPatch children (Patch.Replace old_list new_list))) fl =
  fl & with_old_list
     & with_new_list
     -- & addParent

    where
      -- Old list get -1 score
      -- New list get +1 score
      -- Hence others lists lay around 0 score
      with_old_list   = flc_scores . at t %~ (score old_list (-1))
      with_new_list   = flc_scores . at t %~ (score new_list ( 1))

      score list n m = (Just mempty <> m)
                     & _Just
                     . fls_listType
                     . at list
                     %~ (<> Just n)

{-
      addParent = flc_scores . at t %~ (score MapTerm  1)


      parent term n m = (Just mempty <> m)
                     & _Just
                     . fls_listType
                     . at list
                     %~ (<> Just n)
-}




