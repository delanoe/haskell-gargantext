{-|
Module      : Gargantext.Core.Text.List.WithScores
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Core.Text.List.Group.WithScores
  where

import Control.Lens (makeLenses, set, over, view)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social.Scores
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map


------------------------------------------------------------------------
-- | Main Types
data GroupedWithListScores =
  GroupedWithListScores { _gwls_children :: !(Set Text)
                        , _gwls_listType :: !(Maybe ListType)
                        }
makeLenses ''GroupedWithListScores


data GroupedTextScores score =
  GroupedTextScores { _gts_listType :: !(Maybe ListType)
                    , _gts_score    :: score
                    , _gts_children :: !(Set Text)
                    }
makeLenses 'GroupedTextScores

------------------------------------------------------------------------
-- | Main function
groupWithScores :: Map Text FlowListScores
                -> Map Text (Set NodeId)
                -> Map Text (GroupedTextScores (Set NodeId))
groupWithScores scores =
  Map.mapWithKey (\k a -> scoresToGroupedTextScores
                         (Map.lookup k $ toGroupedWithListScores scores)
                         k a
                 )

  where
    scoresToGroupedTextScores :: Maybe GroupedWithListScores
                              -> Text -> Set NodeId
                              -> GroupedTextScores (Set NodeId)
    scoresToGroupedTextScores Nothing  _ ns = GroupedTextScores Nothing ns Set.empty
    scoresToGroupedTextScores (Just g) t ns = GroupedTextScores list ns (Set.singleton t)
      where
        list = view gwls_listType g
------------------------------------------------------------------------
toGroupedWithListScores :: Map Text FlowListScores -> Map Text GroupedWithListScores
toGroupedWithListScores ms = foldl' (toGroup ms) Map.empty (Map.toList ms)
  where
    toGroup :: Map Text FlowListScores
            -> Map Text GroupedWithListScores
            -> (Text, FlowListScores)
            -> Map Text GroupedWithListScores
    toGroup _ result (t,fs) = case (keyWithMaxValue $ view flc_parents fs) of
      Nothing     -> Map.alter (addGroupedParent (t,fs)) t  result
      Just parent -> Map.alter (addGroupedChild  (t,fs)) parent result


    addGroupedParent :: (Text, FlowListScores)
                     -> Maybe GroupedWithListScores
                     -> Maybe GroupedWithListScores
    addGroupedParent (_,fs) Nothing = Just $ GroupedWithListScores Set.empty list
      where
        list = keyWithMaxValue $ view flc_lists fs

    addGroupedParent (t,fs) (Just g) = Just $ set  gwls_listType list
                                            $ over gwls_children (Set.insert t) g
      where
        list     = keyWithMaxValue $ view flc_lists fs


    addGroupedChild :: (Text, FlowListScores)
                    -> Maybe GroupedWithListScores
                    -> Maybe GroupedWithListScores
    addGroupedChild (t,fs) Nothing  = Just $ GroupedWithListScores (Set.singleton t) list
      where
        list     = keyWithMaxValue $ view flc_lists fs

    addGroupedChild (t,fs) (Just g) = Just $ over gwls_listType (<> list)
                                           $ over gwls_children (Set.insert t) g
      where
        list     = keyWithMaxValue $ view flc_lists fs

