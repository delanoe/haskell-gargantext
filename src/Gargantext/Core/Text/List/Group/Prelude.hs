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
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import Gargantext.API.Ngrams.Types (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
import Gargantext.Core.Text.Metrics (Scored(..), scored_genInc)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

type Stem = NgramsTerm
------------------------------------------------------------------------
-- | Main Types to group With Scores but preserving Tree dependencies
-- Therefore there is a need of Tree of GroupedTextScores
-- to target continuation type for the flow (FlowCont Text GroupedTreeScores)
data GroupedTreeScores score =
  GroupedTreeScores { _gts'_listType :: !(Maybe ListType)
                    , _gts'_children :: !(HashMap NgramsTerm (GroupedTreeScores score))
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
  hasTerms :: a -> Set NgramsTerm

------------------------------------------------------------------------
-- | Instances declartion for (GroupedTreeScores a)
instance ViewListType (GroupedTreeScores a) where
  viewListType = view gts'_listType

instance SetListType (GroupedTreeScores a) where
  setListType lt g = over gts'_children (setListType lt)
                $ set gts'_listType lt g

instance SetListType (HashMap NgramsTerm (GroupedTreeScores a)) where
  setListType lt = HashMap.map (set gts'_listType lt)

                            ------

instance ViewScore (GroupedTreeScores Double) Double where
  viewScore = viewScores

instance ViewScores (GroupedTreeScores Double) Double where
  viewScores g = sum $ parent : children
    where
      parent   = view gts'_score g
      children = map viewScores $ HashMap.elems $ view gts'_children g


instance ViewScore (GroupedTreeScores (Set NodeId)) Int where
  viewScore = Set.size . viewScores

instance ViewScores (GroupedTreeScores (Set NodeId)) (Set NodeId) where
  viewScores g = Set.unions $ parent : children
    where
      parent   = view gts'_score g
      children = map viewScores $ HashMap.elems $ view gts'_children g


instance ViewScore (GroupedTreeScores (Scored NgramsTerm)) Double where
  viewScore = view (gts'_score . scored_genInc)

                            ------
instance HasTerms (HashMap NgramsTerm (GroupedTreeScores a)) where
  hasTerms = Set.unions . (map hasTerms) . HashMap.toList

instance HasTerms (NgramsTerm, GroupedTreeScores a) where
  hasTerms (t, g) = Set.singleton t  <> children
    where
      children = Set.unions
               $ map hasTerms
               $ HashMap.toList
               $ view gts'_children g

                            ------

instance ToNgramsElement (HashMap NgramsTerm (GroupedTreeScores a)) where
  toNgramsElement = List.concat . (map toNgramsElement) . HashMap.toList


instance ToNgramsElement (NgramsTerm, GroupedTreeScores a) where
  toNgramsElement (t, gts) = parent : children
    where
      parent = mkNgramsElement t
                               (fromMaybe CandidateTerm $ viewListType gts)
                               Nothing
                               (mSetFromList $ HashMap.keys
                                             $ view gts'_children gts
                               )
      children = List.concat
               $ map (childrenWith t t)
               $ HashMap.toList
               $ view gts'_children gts

      childrenWith root parent' (t', gts') = parent'' : children'
        where
          parent''   = mkNgramsElement t'
                                      (fromMaybe CandidateTerm $ viewListType gts')
                                      (Just $ RootParent root parent')
                                      (mSetFromList $ HashMap.keys
                                                    $ view gts'_children gts'
                                      )
          children' = List.concat
                    $ map (childrenWith root t' )
                    $ HashMap.toList
                    $ view gts'_children gts'


