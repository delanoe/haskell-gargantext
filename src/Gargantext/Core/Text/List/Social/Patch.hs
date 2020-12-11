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
import Data.Monoid
import Control.Lens hiding (cons)
import Gargantext.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main (ListType)
import qualified Data.Patch.Class as Patch (Replace(..))

{-
fromList [(NgramsTerms,fromList [(NodeId 189,
-}

addScorePatch :: (NgramsTerm , NgramsPatch)
              -> FlowCont Text FlowListScores
              -> FlowCont Text FlowListScores
{- | Case of changing listType only. Patches look like:

This patch move "problem" from MapTerm to CandidateTerm
,fromList [(NgramsTerm {unNgramsTerm = "problem"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = CandidateTerm}})]

This patch move "paper" from MapTerm to StopTerm
,fromList [(NgramsTerm {unNgramsTerm = "paper"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = StopTerm}})]])])]

Children are not modified in this specific case.
-}
-- | Old list get -1 score
-- New list get +1 score
-- Hence others lists lay around 0 score
-- TODO add children
addScorePatch (NgramsTerm t, (NgramsPatch _children (Patch.Replace old_list new_list))) fl =
  fl & flc_scores . at t %~ (score fls_listType old_list (-1))
     & flc_scores . at t %~ (score fls_listType new_list ( 1))
     
{-
[fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Just (), _new = Nothing})])), _patch_list = Keep})]
,fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Nothing, _new = Just ()})])), _patch_list = Keep})]
-}
addScorePatch (NgramsTerm t, NgramsPatch children Patch.Keep) fl = undefined
{-
      addParent = flc_scores . at t %~ (score MapTerm  1)


      parent term n m = (Just mempty <> m)
                     & _Just
                     . fls_listType
                     . at list
                     %~ (<> Just n)
-}



{- | Inserting a new Ngrams
fromList [(NgramsTerm {unNgramsTerm = "journal"},NgramsReplace {_patch_old = Nothing, _patch_new = Just (NgramsRepoElement {_nre_size = 1, _nre_list = CandidateTerm, _nre_root = Nothing, _nre_parent = Nothing, _nre_children = MSet (fromList [])})})],f
-}
addScorePatch (NgramsTerm t, NgramsReplace _ (Just nre)) fl        =
  fl & flc_scores . at t %~ (score fls_listType $ nre ^. nre_list) ( 1)


-- score :: ListType -> Int -> Maybe FlowListScores -> Maybe FlowListScores
score field list n m = (Just mempty <> m)
               & _Just
               . field
               . at list
               %~ (<> Just n)


