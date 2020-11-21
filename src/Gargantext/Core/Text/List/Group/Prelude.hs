{-|
Module      : Gargantext.Core.Text.List.Group.Prelude
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Core.Text.List.Group.Prelude
  where

import Control.Lens (makeLenses)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude
import qualified Data.Set as Set

------------------------------------------------------------------------
-- | Group With Scores Main Types
-- Tree of GroupedTextScores
-- Target : type FlowCont Text GroupedTextScores'
data GroupedTextScores' score =
  GroupedTextScores' { _gts'_listType :: !(Maybe ListType)
                     , _gts'_children :: !(Set (GroupedTextScores' score))
                     , _gts'_score    :: score
                     } deriving (Show, Ord, Eq)

instance (Semigroup a, Ord a) => Semigroup (GroupedTextScores' a) where
  (<>) (GroupedTextScores'  l1 s1 c1)
       (GroupedTextScores'  l2 s2 c2)
      = GroupedTextScores' (l1 <> l2)
                           (s1 <> s2)
                           (c1 <> c2)

instance (Ord score, Monoid score)
  => Monoid (GroupedTextScores' score) where
    mempty = GroupedTextScores' Nothing Set.empty mempty

makeLenses 'GroupedTextScores'

-- | Intermediary Type
data GroupedWithListScores =
  GroupedWithListScores { _gwls_listType :: !(Maybe ListType)
                        , _gwls_children :: !(Set Text)
                        } deriving (Show)

instance Semigroup GroupedWithListScores where
  (<>) (GroupedWithListScores c1 l1)
       (GroupedWithListScores c2 l2) =
        GroupedWithListScores (c1 <> c2)
                              (l1 <> l2)

instance Monoid GroupedWithListScores where
  mempty = GroupedWithListScores Nothing Set.empty

makeLenses ''GroupedWithListScores

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Group With Stem Main Types
type Stem  = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Text
              , _gt_score    :: !score
              , _gt_children :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem -- needed ?
              , _gt_nodes    :: !(Set NodeId)
              }  deriving (Show, Eq) --}
{-
instance Show score => Show (GroupedText score) where
  show (GroupedText lt l s _ _ _ _) = show l <> " : " <> show lt <> " : " <> show s
--}

{-
instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _ _)
       (GroupedText _ _ score2 _ _ _ _) = (==) score1 score2
-}

instance (Eq a, Ord a) => Ord (GroupedText a) where
  compare (GroupedText _ _ score1 _ _ _ _)
          (GroupedText _ _ score2 _ _ _ _) = compare score1 score2

instance Ord a => Semigroup (GroupedText a) where
  (<>) (GroupedText lt1 label1 score1 group1 s1 stem1 nodes1)
        (GroupedText lt2 label2 score2 group2 s2 stem2 nodes2)
          | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr) s1 stem1 nodes
          | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr) s2 stem2 nodes
    where
      lt = lt1 <> lt2
      gr    = Set.union group1 group2
      nodes = Set.union nodes1 nodes2

-- | Lenses Instances
makeLenses 'GroupedText

