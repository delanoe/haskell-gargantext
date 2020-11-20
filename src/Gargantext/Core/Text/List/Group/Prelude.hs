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
makeLenses 'GroupedTextScores'
instance (Semigroup a, Ord a) => Semigroup (GroupedTextScores' a) where
  (<>) (GroupedTextScores' l1 s1 c1)
       (GroupedTextScores' l2 s2 c2)
    = GroupedTextScores' (l1 <> l2) (s1 <> s2) (c1 <> c2)


-- | Intermediary Type
data GroupedWithListScores =
  GroupedWithListScores { _gwls_listType :: !(Maybe ListType)
                        , _gwls_children :: !(Set Text)
                        } deriving (Show)
makeLenses ''GroupedWithListScores
instance Semigroup GroupedWithListScores where
  (<>) (GroupedWithListScores c1 l1)
       (GroupedWithListScores c2 l2) =
        GroupedWithListScores (c1 <> c2)
                              (l1 <> l2)

instance Monoid GroupedWithListScores where
  mempty = GroupedWithListScores Nothing Set.empty

------------------------------------------------------------------------
-- | Group With Stem Main Types
