{-|
Module      : Gargantext.Core.Text.Types
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Text.Types
  where

import Control.Lens (makeLenses)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

------------------------------------------------------------------------------
hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
hasListType m (GroupedText _ label _ g _ _ _) =
  List.foldl' (<>) Nothing
  $ map (\t -> Map.lookup t m)
  $ Set.toList
  $ Set.insert label g


------------------------------------------------------------------------------
type Group = Lang -> Int -> Int -> Text -> Text
type Stem  = Text
type Label = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Label
              , _gt_score    :: !score
              , _gt_group    :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem
              , _gt_nodes    :: !(Set NodeId)
              }
instance Show score => Show (GroupedText score) where
  show (GroupedText _ l s _ _ _ _) = show l <> ":" <> show s

instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _ _)
       (GroupedText _ _ score2 _ _ _ _) = (==) score1 score2

instance (Eq a, Ord a) => Ord (GroupedText a) where
  compare (GroupedText _ _ score1 _ _ _ _)
          (GroupedText _ _ score2 _ _ _ _) = compare score1 score2



-- Lenses Instances
makeLenses 'GroupedText
