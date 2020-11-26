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








-- 8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--
-- TODO to remove below
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

-- | Lenses Instances
makeLenses 'GroupedText

instance ViewListType (GroupedText a) where
  viewListType = view gt_listType

instance SetListType (GroupedText a) where
  setListType = set gt_listType

instance Ord a => ViewScore (GroupedText a) a where
  viewScore = (view gt_score)

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

instance SetListType [GroupedText Int] where
  setListType lt = map (setListType lt)


instance ToNgramsElement (Map Stem (GroupedText Int)) where
  toNgramsElement = List.concat . (map toNgramsElement) . Map.elems


instance ToNgramsElement [GroupedText a] where
  toNgramsElement = List.concat . (map toNgramsElement)

instance ToNgramsElement (GroupedText a) where
  toNgramsElement :: GroupedText a -> [NgramsElement]
  toNgramsElement (GroupedText listType label _ setNgrams _ _ _) =
    [parentElem] <> childrenElems
      where
        parent = label
        children = Set.toList setNgrams
        parentElem    = mkNgramsElement (NgramsTerm parent)
                                        (fromMaybe CandidateTerm listType)
                                        Nothing
                                        (mSetFromList (NgramsTerm <$> children))
        childrenElems = map (\t -> mkNgramsElement t (fromMaybe CandidateTerm $ listType)
                                                   (Just $ RootParent (NgramsTerm parent) (NgramsTerm parent))
                                                   (mSetFromList [])
                            ) (NgramsTerm <$> children)


-- 8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--
