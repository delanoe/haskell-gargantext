{-|
Module      : Gargantext.Core.Text.List.Group
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}


module Gargantext.Core.Text.List.Group
  where

import Data.Maybe (fromMaybe)
import Control.Lens (makeLenses, set, (^.))
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup (Semigroup, (<>))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social.Scores (FlowListScores(..), flc_lists, flc_parents, keyWithMaxValue)
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Text as Text

{-
data NgramsListBuilder = BuilderStepO { stemSize :: !Int
                                      , stemX    :: !Int
                                      , stopSize :: !StopSize
                                      }
                       | BuilderStep1 { withModel :: !Model }
                       | BuilderStepN { withModel :: !Model }
                       | Tficf { nlb_lang           :: !Lang
                               , nlb_group1         :: !Int
                               , nlb_group2         :: !Int
                               , nlb_stopSize       :: !StopSize
                               , nlb_userCorpusId   :: !UserCorpusId
                               , nlb_masterCorpusId :: !MasterCorpusId
                               }
-}

data StopSize = StopSize {unStopSize :: !Int}

-- | TODO: group with 2 terms only can be
-- discussed. Main purpose of this is offering
-- a first grouping option to user and get some
-- enriched data to better learn and improve that algo
data GroupParams = GroupParams { unGroupParams_lang  :: !Lang
                               , unGroupParams_len   :: !Int
                               , unGroupParams_limit :: !Int
                               , unGroupParams_stopSize :: !StopSize
                               }
                 | GroupIdentity

ngramsGroup :: GroupParams
            -> Text
            -> Text
ngramsGroup GroupIdentity  = identity
ngramsGroup (GroupParams l _m _n _) = 
                    Text.intercalate " "
                  . map (stem l)
                  -- . take n
                  . List.sort
                  -- . (List.filter (\t -> Text.length t > m))
                  . Text.splitOn " "
                  . Text.replace "-" " "

------------------------------------------------------------------------
data GroupedTextParams a b =
  GroupedTextParams { _gt_fun_stem    :: Text -> Text
                    , _gt_fun_score   :: a -> b
                    , _gt_fun_texts   :: a -> Set Text
                    , _gt_fun_nodeIds :: a -> Set NodeId
                    -- , _gt_fun_size    :: a -> Int
                    }

makeLenses 'GroupedTextParams

groupedTextWithStem :: Ord b
              => GroupedTextParams a b
              -> Map Text a
              -> Map Stem (GroupedText b)
groupedTextWithStem gparams from =
  Map.fromListWith (<>) $ map (group gparams) $ Map.toList from
    where
      group gparams' (t,d) = let t' = (gparams' ^. gt_fun_stem) t
                     in (t', GroupedText
                                Nothing
                                t
                                ((gparams' ^. gt_fun_score)   d)
                                ((gparams' ^. gt_fun_texts)   d)
                                (size        t)
                                t'
                                ((gparams' ^. gt_fun_nodeIds) d)
                         )

------------------------------------------------------------------------
------------------------------------------------------------------------
type Stem  = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Text
              , _gt_score    :: !score
              , _gt_children :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem -- needed ?
              , _gt_nodes    :: !(Set NodeId)
              } {-deriving Show--}
--{-
instance Show score => Show (GroupedText score) where
  show (GroupedText lt l s _ _ _ _) = show l <> " : " <> show lt <> " : " <> show s
--}

instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _ _)
       (GroupedText _ _ score2 _ _ _ _) = (==) score1 score2

instance (Eq a, Ord a) => Ord (GroupedText a) where
  compare (GroupedText _ _ score1 _ _ _ _)
          (GroupedText _ _ score2 _ _ _ _) = compare score1 score2

-- | Lenses Instances
makeLenses 'GroupedText


------------------------------------------------------------------------
instance WithParent (Text, Set NodeId) (Text, Set NodeId) where
  selfParent (t, (_,n)) = (t, n)

instance HasNgrams (Text, Set NodeId) where
  hasNgrams (t, _) = t

-- instance HasGroupWithScores (Text, Set NodeId) Int where
createGroupWithScores' fs (t, ns)   = GroupedText (keyWithMaxValue $ fs ^. flc_lists)
                                           label
                                           (Set.size ns)
                                           children
                                           (size t)
                                           t
                                           ns
  where
    (label, children) =  case keyWithMaxValue $ fs ^. flc_parents of
                          Nothing -> (t, Set.empty)
                          Just t' -> (t', Set.singleton t)

updateGroupWithScores' fs (t, ns) g = set gt_listType (keyWithMaxValue $ fs ^. flc_lists)
                             $ set gt_nodes (Set.union ns $ g ^. gt_nodes) g

------------------------------------------------------------------------





toGroupedText :: {-( FlowList c a b
                  Ord b
                 )
              => -} GroupedTextParams a b
              -> Map Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Stem (GroupedText Int)
toGroupedText groupParams scores =
  (groupWithStem groupParams) . (groupWithScores scores)


groupWithStem :: {- ( HasNgrams a
                 , HasGroupWithScores a b
                 , Semigroup a
                 ,  Ord b
                 ) 
              => -} GroupedTextParams a b
              -> ([(Text, Set NodeId)], Map Text (GroupedText Int))
              -> Map Stem (GroupedText Int)
groupWithStem _ = snd -- TODO (just for tests on Others Ngrams which do not need stem)

withParent' :: Map Text c -> Text -> a -> a
withParent' = undefined

groupWithScores :: {- Ord b -- (FlowList c a b, Ord b)
                => -} Map Text FlowListScores
                -> Map Text (Set NodeId)
                -> ([(Text, Set NodeId)],  Map Text (GroupedText Int))
groupWithScores scores ms' = foldl' fun_group start ms
  where
    start = ([], Map.empty)
    ms    = map (\(t, ns) -> (t, ns)) (Map.toList ms')

    fun_group :: ([(Text, Set NodeId)], Map Text (GroupedText Int)) -> (Text, Set NodeId)
                -> ([(Text, Set NodeId)],  Map Text (GroupedText Int))
    fun_group (left, grouped) current =
      case Map.lookup (fst current) scores of
        Just scores' ->
          case keyWithMaxValue $ scores' ^. flc_parents of
            Nothing     -> (left, Map.alter (updateWith scores' current) (fst current) grouped)
            Just parent -> fun_group (left, grouped) (withParent' ms' parent current)
        Nothing      -> (current : left, grouped)

    updateWith scores current Nothing  = Just $ createGroupWithScores' scores current
    updateWith scores current (Just x) = Just $ updateGroupWithScores' scores current x

------------------------------------------------------------------------
type FlowList c a b = (HasNgrams a, HasGroupWithScores a b, WithParent c a, Semigroup a)

class HasNgrams a where
  hasNgrams :: a -> Text

class HasGroup a b | a -> b where
  hasGroup :: a -> GroupedText b

class HasGroupWithStem a b where
  hasGroupWithStem :: GroupedTextParams a b -> Map Text a -> Map Stem (GroupedText b)

class HasGroupWithScores a b | a -> b where
  createGroupWithScores :: FlowListScores -> a -> GroupedText b
  updateGroupWithScores :: FlowListScores -> a -> GroupedText b -> GroupedText b

class WithParent c a | c -> a where
  selfParent :: (Text, c) -> a
  withParent :: Map Text c -> Text -> a -> a


------------------------------------------------------------------------
instance Ord a => Semigroup (GroupedText a) where
  (<>) (GroupedText lt1 label1 score1 group1 s1 stem1 nodes1)
        (GroupedText lt2 label2 score2 group2 s2 stem2 nodes2)
          | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr) s1 stem1 nodes
          | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr) s2 stem2 nodes
    where
      lt = lt1 <> lt2
      gr    = Set.union group1 group2
      nodes = Set.union nodes1 nodes2


------------------------------------------------------------------------
-- to remove
-- | These instances seeems useless, just for debug purpose
instance HasNgrams (Set Text, Set NodeId) where
  hasNgrams  = fromMaybe "Nothing" . head . Set.elems . fst

instance HasGroupWithScores (Set Text, Set NodeId) Int where
  createGroupWithScores = undefined
  updateGroupWithScores = undefined


------------------------------------------------------------------------
-- | To be removed
addListType :: Map Text ListType -> GroupedText a -> GroupedText a
addListType m g = set gt_listType (hasListType m g) g
  where
    hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
    hasListType m' (GroupedText _ label _ g' _ _ _) =
        List.foldl' (<>) Nothing
      $ map (\t -> Map.lookup t m')
      $ Set.toList
      $ Set.insert label g'
