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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Group.Prelude
  where

import Control.Lens (makeLenses, view, set, over)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.Metrics (scored', Scored(..), scored_speExc, scored_genInc, normalizeGlobal, normalizeLocal)
import Gargantext.API.Ngrams.Types (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

type Stem = Text
------------------------------------------------------------------------
-- | Main Types to group With Scores but preserving Tree dependencies
-- Therefore there is a need of Tree of GroupedTextScores
-- to target continuation type for the flow (FlowCont Text GroupedTreeScores)
data GroupedTreeScores score =
  GroupedTreeScores { _gts'_listType :: !(Maybe ListType)
                    , _gts'_children :: !(Map Text (GroupedTreeScores score))
                    , _gts'_score    :: !score
                    } deriving (Show, Ord, Eq)

instance (Semigroup a) => Semigroup (GroupedTreeScores a) where
  (<>) (GroupedTreeScores  l1 s1 c1)
       (GroupedTreeScores  l2 s2 c2)
      = GroupedTreeScores (l1 <> l2)
                          (s1 <> s2)
                          (c1 <> c2)

instance (Ord score, Monoid score)
  => Monoid (GroupedTreeScores score) where
    mempty = GroupedTreeScores mempty mempty mempty

makeLenses 'GroupedTreeScores

------------------------------------------------------------------------
-- | Main Classes
class ViewListType a where
  viewListType :: a -> Maybe ListType

class SetListType a where
  setListType :: Maybe ListType -> a -> a

------
class Ord b => ViewScore a b | a -> b where
  viewScore :: a -> b

class ViewScores a b | a -> b where
  viewScores :: a -> b

--------
class ToNgramsElement a where
  toNgramsElement :: a -> [NgramsElement]

class HasTerms a where
  hasTerms :: a -> Set Text

------------------------------------------------------------------------
-- | Instances declartion for (GroupedTreeScores a)
instance ViewListType (GroupedTreeScores a) where
  viewListType = view gts'_listType

instance SetListType (GroupedTreeScores a) where
  setListType lt g = over gts'_children (setListType lt)
                $ set gts'_listType lt g

instance SetListType (Map Text (GroupedTreeScores a)) where
  setListType lt = Map.map (set gts'_listType lt)

                            ------

instance ViewScore (GroupedTreeScores Double) Double where
  viewScore = viewScores

instance ViewScores (GroupedTreeScores Double) Double where
  viewScores g = sum $ parent : children
    where
      parent   = view gts'_score g
      children = map viewScores $ Map.elems $ view gts'_children g


instance ViewScore (GroupedTreeScores (Set NodeId)) Int where
  viewScore = Set.size . viewScores

instance ViewScores (GroupedTreeScores (Set NodeId)) (Set NodeId) where
  viewScores g = Set.unions $ parent : children
    where
      parent   = view gts'_score g
      children = map viewScores $ Map.elems $ view gts'_children g


instance ViewScore (GroupedTreeScores (Scored Text)) Double where
  viewScore = view (gts'_score . scored_genInc)

                            ------
instance HasTerms (Map Text (GroupedTreeScores a)) where
  hasTerms = Set.unions . (map hasTerms) . Map.toList

instance HasTerms (Text, GroupedTreeScores a) where
  hasTerms (t, g) = Set.singleton t  <> children
    where
      children = Set.unions
               $ map hasTerms
               $ Map.toList
               $ view gts'_children g

                            ------

instance ToNgramsElement (Map Text (GroupedTreeScores a)) where
  toNgramsElement = List.concat . (map toNgramsElement) . Map.toList


instance ToNgramsElement (Text, GroupedTreeScores a) where
  toNgramsElement (t, gts) = parent : children
    where
      parent = mkNgramsElement (NgramsTerm t)
                               (fromMaybe CandidateTerm $ viewListType gts)
                               Nothing
                               (mSetFromList $ map NgramsTerm
                                             $ Map.keys
                                             $ view gts'_children gts
                               )
      children = List.concat
               $ map (childrenWith (NgramsTerm t) (NgramsTerm t) )
               $ Map.toList
               $ view gts'_children gts

      childrenWith root parent' (t', gts') = parent'' : children'
        where
          parent''   = mkNgramsElement (NgramsTerm t')
                                      (fromMaybe CandidateTerm $ viewListType gts')
                                      (Just $ RootParent root parent')
                                      (mSetFromList $ map NgramsTerm
                                                    $ Map.keys
                                                    $ view gts'_children gts'
                                      )
          children' = List.concat
                    $ map (childrenWith root (NgramsTerm t') )
                    $ Map.toList
                    $ view gts'_children gts'


