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

import Data.Maybe (fromMaybe)
import Control.Lens (makeLenses, set, (^.), (%~))
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup (Semigroup, (<>))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social.Scores
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Text as Text




data GroupedWithListScores =
  GroupedWithListScores { _gwls_children :: !(Set Text)
                        , _gwls_listType :: !(Maybe ListType)
                        }
makeLenses ''GroupedWithListScores

toGroupedWithListScores :: Map Text FlowListScores -> Map Text GroupedWithListScores
toGroupedWithListScores ms = foldl' (toGroup ms) Map.empty (Map.toList ms)
  where
    toGroup :: Map Text FlowListScores
            -> Map Text GroupedWithListScores
            -> (Text, FlowListScores)
            -> Map Text GroupedWithListScores
    toGroup ms' result (t,fs) = case (keyWithMaxValue $ fs ^. flc_parents) of
      Nothing     -> Map.alter (addGroupedParent (t,fs)) t  result
      Just parent -> Map.alter (addGroupedChild  (t,fs)) parent result


addGroupedParent :: (Text, FlowListScores) -> Maybe GroupedWithListScores -> Maybe GroupedWithListScores
addGroupedParent (_,fs) Nothing = Just $ GroupedWithListScores Set.empty list
  where
    list = keyWithMaxValue $ fs ^. flc_lists

addGroupedParent (t,fs) (Just g) = Just $ set gwls_listType list
                                        $ (%~) gwls_children (Set.insert t) g
  where
    list     = keyWithMaxValue $ fs ^. flc_lists


addGroupedChild :: (Text, FlowListScores) -> Maybe GroupedWithListScores -> Maybe GroupedWithListScores
addGroupedChild (t,fs) Nothing  = Just $ GroupedWithListScores (Set.singleton t) list
  where
    list     = keyWithMaxValue $ fs ^. flc_lists

addGroupedChild (t,fs) (Just g) = Just $ (%~) gwls_listType (<> list)
                                       $ (%~) gwls_children (Set.insert t) g
  where
    list     = keyWithMaxValue $ fs ^. flc_lists


data GroupedTextScores score =
  GroupedTextScores { _gts_listType :: !(Maybe ListType)
                    , _gts_score    :: score
                    , _gts_children :: !(Set Text)
                    }
makeLenses 'GroupedTextScores



groupWithScores :: Map Text FlowListScores
                -> Map Text (Set NodeId)
                -> Map Text (GroupedTextScores (Set NodeId))
groupWithScores scores =
  Map.mapWithKey (\k a -> scoresToGroupedTextScores
                         (Map.lookup k $ toGroupedWithListScores scores)
                         k a
                 )

scoresToGroupedTextScores :: Maybe GroupedWithListScores
                          -> Text -> Set NodeId
                          -> GroupedTextScores (Set NodeId)
scoresToGroupedTextScores Nothing t ns  = undefined
scoresToGroupedTextScores (Just g) t ns = undefined


