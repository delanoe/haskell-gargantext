{-|
Module      : Gargantext.Core.Text.Group
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


module Gargantext.Core.Text.Group
  where

import Control.Lens (makeLenses, set, (^.))
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social.Group (FlowListScores(..), flc_lists)
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
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
mergeMapParent :: Map Text (GroupedText b)
               -> Map Text (Map Text Int)
               -> Map Text (GroupedText b)
mergeMapParent = undefined

------------------------------------------------------------------------
toGroupedText :: Ord b
              => (Text -> Text      )
              -> (a    -> b         )
              -> (a    -> Set Text  )
              -> (a    -> Set NodeId)
              -> [(Text,a)]
              -> Map Stem (GroupedText b)
toGroupedText fun_stem fun_score fun_texts fun_nodeIds from =
  Map.fromListWith grouping $ map group from
    where
      group (t,d) = let t' = fun_stem t
                     in (t', GroupedText
                                Nothing
                                t
                                (fun_score   d)
                                (fun_texts   d)
                                (size        t)
                                t'
                                (fun_nodeIds d)
                         )

grouping :: Ord a
         => GroupedText a
         -> GroupedText a
         -> GroupedText a
grouping (GroupedText lt1 label1 score1 group1 s1 stem1 nodes1)
         (GroupedText lt2 label2 score2 group2 s2 stem2 nodes2)
         | score1 >= score2 = GroupedText lt label1 score1 (Set.insert label2 gr) s1 stem1 nodes
         | otherwise        = GroupedText lt label2 score2 (Set.insert label1 gr) s2 stem2 nodes
    where
      lt = lt1 <> lt2
      gr    = Set.union group1 group2
      nodes = Set.union nodes1 nodes2

------------------------------------------------------------------------
toGroupedText_FlowListScores :: ( FlowList a b
                                , Ord a
                                )
                             => [a]
                             -> Map Text FlowListScores
                             -> Map Text (GroupedText b)
toGroupedText_FlowListScores = undefined


toGroupedText_FlowListScores' :: ( FlowList a b
                                 )
                              => [a]
                              -> Map Text FlowListScores
                              -> ( [a]
                                 ,  Map Text (GroupedText b)
                                 )
toGroupedText_FlowListScores' ms mf = foldl' fun_group start ms
  where
    start = ([], Map.empty)
    fun_group (left, grouped) current =
      case Map.lookup (hasNgrams current) mf of
        Just scores -> (left, Map.alter (updateWith scores current) (hasNgrams current) grouped)
        Nothing     -> (current : left, grouped)
    updateWith scores current Nothing  = Just $ createGroupWith scores current
    updateWith scores current (Just x) = Just $ updateGroupWith scores current x

type FlowList a b = (HasNgrams a, HasGroup a b)

class HasNgrams a where
  hasNgrams :: a -> Text

class HasGroup a b | a -> b where
  createGroupWith :: FlowListScores -> a -> GroupedText b
  updateGroupWith :: FlowListScores -> a
                  -> GroupedText b
                  -> GroupedText b

------------------------------------------
instance HasGroup (Text, Set NodeId) Int where
  createGroupWith fs (t, ns)   = GroupedText (mapMax $ fs ^. flc_lists)
                                             t
                                             (Set.size ns)
                                             Set.empty
                                             (size t)
                                             t
                                             ns
  updateGroupWith fs (t, ns) g = undefined

mapMax :: Map a b -> Maybe a
mapMax m = (fst . fst) <$> Map.maxViewWithKey m
------------------------------------------------------------------------
type Stem  = Text
type Label = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Label
              , _gt_score    :: !score
              , _gt_children :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem
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

-- Lenses Instances
makeLenses 'GroupedText

------------------------------------------------------------------------
addListType :: Map Text ListType -> GroupedText a -> GroupedText a
addListType m g = set gt_listType (hasListType m g) g
  where
    hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
    hasListType m' (GroupedText _ label _ g' _ _ _) =
        List.foldl' (<>) Nothing
      $ map (\t -> Map.lookup t m')
      $ Set.toList
      $ Set.insert label g'
