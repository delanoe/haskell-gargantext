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
import Data.Monoid
import Data.Semigroup
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types (ListId)
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
addScorePatch fl (NgramsTerm t, (NgramsPatch children' (Patch.Replace old_list new_list))) =
  -- | Adding New Children score
  addScorePatch fl' (NgramsTerm t, NgramsPatch children' Patch.Keep)
    where
      -- | Adding New ListType score
      fl' = fl & flc_scores . at t %~ (score fls_listType old_list (-1))
          & flc_scores . at t %~ (score fls_listType new_list ( 1))

-- | Patching existing Ngrams with children
addScorePatch fl (NgramsTerm p, NgramsPatch children' Patch.Keep) =
  foldl' add' fl $ patchMSet_toList children'
    where
      -- | Adding a child
      add' fl' (NgramsTerm t, Patch.Replace Nothing (Just _)) = doLink ( 1) p t fl'
      -- | Removing a child
      add' fl' (NgramsTerm t, Patch.Replace (Just _) Nothing) = doLink (-1) p t fl'

      -- | This case should not happen: does Nothing
      add' fl' _ = fl'

-- | Inserting a new Ngrams
addScorePatch fl (NgramsTerm t, NgramsReplace Nothing (Just nre)) =
  childrenScore 1 t (nre ^. nre_children) 
  $ fl & flc_scores . at t %~ (score fls_listType $ nre ^. nre_list) 1

addScorePatch fl (NgramsTerm t, NgramsReplace (Just old_nre) maybe_new_nre) =
  let fl' = childrenScore (-1) t (old_nre ^. nre_children) 
            $ fl & flc_scores . at t %~ (score fls_listType $ old_nre ^. nre_list) (-1)
    in case maybe_new_nre of
      Nothing      -> fl'
      Just new_nre -> addScorePatch fl' (NgramsTerm t, NgramsReplace Nothing (Just new_nre))

addScorePatch fl (NgramsTerm _, NgramsReplace Nothing Nothing) = fl

-------------------------------------------------------------------------------
-- | Utils
childrenScore :: Int
              -> Text
              -> MSet NgramsTerm
              -> FlowCont Text FlowListScores
              -> FlowCont Text FlowListScores
childrenScore n parent children' fl =
  foldl' add' fl $ unMSet children'
    where
        add' fl' (NgramsTerm t) = doLink n parent t fl'

------------------------------------------------------------------------
doLink :: Ord a
       => Int
       -> Text
       -> a
       -> FlowCont a FlowListScores
       -> FlowCont a FlowListScores
doLink n parent child fl' = fl' & flc_scores . at child %~ (score fls_parents parent n)


score :: (Monoid a, At m, Semigroup (IxValue m))
      => ((m -> Identity m) -> a -> Identity b)
      -> Index m -> IxValue m -> Maybe a -> Maybe b
score field list n m = (Just mempty <> m)
                     & _Just
                     . field
                     . at list
                     %~ (<> Just n)

------------------------------------------------------------------------
patchMSet_toList :: Ord a => PatchMSet a -> [(a,AddRem)]
patchMSet_toList = Map.toList . unPatchMap . unPatchMSet

unMSet :: MSet a -> [a]
unMSet (MSet a) = Map.keys a

