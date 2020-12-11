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

import Control.Lens hiding (cons)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types (ListId)
import Gargantext.Core.Types.Main (ListType)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Patch.Class as Patch (Replace(..))


addScorePatches :: NgramsType -> [ListId]
               -> FlowCont Text FlowListScores
               -> Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])
               -> FlowCont Text FlowListScores
addScorePatches nt listes fl repo = foldl' (addScorePatchesList nt repo) fl listes


addScorePatchesList :: NgramsType
                   -> Map NgramsType (Map ListId [Map NgramsTerm NgramsPatch])
                   -> FlowCont Text FlowListScores
                   -> ListId
                   -> FlowCont Text FlowListScores
addScorePatchesList nt repo fl lid = foldl' addScorePatch fl patches
  where
    patches = maybe [] (List.concat . (map Map.toList)) patches'

    patches' = do
      lists      <- Map.lookup nt repo
      mapPatches <- Map.lookup lid lists
      pure mapPatches



addScorePatch :: FlowCont Text FlowListScores
              -> (NgramsTerm , NgramsPatch)
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
addScorePatch fl (NgramsTerm t, (NgramsPatch _children (Patch.Replace old_list new_list))) =
  fl & flc_scores . at t %~ (score fls_listType old_list (-1))
     & flc_scores . at t %~ (score fls_listType new_list ( 1))
     
{-
[fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Just (), _new = Nothing})])), _patch_list = Keep})]
,fromList [(NgramsTerm {unNgramsTerm = "approach"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [(NgramsTerm {unNgramsTerm = "order"},Replace {_old = Nothing, _new = Just ()})])), _patch_list = Keep})]


fromList [(NgramsTerm {unNgramsTerm = "Journals"}
   ,NgramsReplace { _patch_old = Nothing
                  , _patch_new = Just (NgramsRepoElement { _nre_size = 1
                                                         , _nre_list = MapTerm
                                                         , _nre_root = Nothing
                                                         , _nre_parent = Nothing, _nre_children = MSet (fromList [(NgramsTerm {unNgramsTerm = "European Journal of Operational Research"},()),(NgramsTerm {unNgramsTerm = "Physical Review C"},())])})})]

,fromList [(NgramsTerm {unNgramsTerm = "NOT FOUND"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = StopTerm}})]])])]

-}
addScorePatch fl (NgramsTerm t, NgramsPatch children Patch.Keep) = foldl' add fl $ toList children
  where
      add fl' (NgramsTerm t, Patch.Replace Nothing (Just _)) = doLink ( 1) t fl'
      add fl' (NgramsTerm t, Patch.Replace (Just _) Nothing) = doLink (-1) t fl'
      add _ _ = panic "addScorePatch: Error should not happen"

      toList :: Ord a => PatchMSet a -> [(a,AddRem)]
      toList = Map.toList . unPatchMap . unPatchMSet

      doLink n child fl' = fl' & flc_scores . at child %~ (score fls_parents child n)


-- | TODO
addScorePatch _ (NgramsTerm _, NgramsReplace _nre Nothing) =
  panic "[G.C.T.L.S.P.addScorePatch] TODO needs nre"

{- | Inserting a new Ngrams
fromList [(NgramsTerm {unNgramsTerm = "journal"},NgramsReplace {_patch_old = Nothing, _patch_new = Just (NgramsRepoElement {_nre_size = 1, _nre_list = CandidateTerm, _nre_root = Nothing, _nre_parent = Nothing, _nre_children = MSet (fromList [])})})],f
-}
addScorePatch fl (NgramsTerm t, NgramsReplace _ (Just nre)) =
  fl & flc_scores . at t %~ (score fls_listType $ nre ^. nre_list) ( 1)


-- score :: ListType -> Int -> Maybe FlowListScores -> Maybe FlowListScores
score field list n m = (Just mempty <> m)
               & _Just
               . field
               . at list
               %~ (<> Just n)
