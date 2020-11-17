{-|
Module      : Gargantext.Core.Text.List.Group.WithStem
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

module Gargantext.Core.Text.List.Group.WithStem
  where

import Control.Lens (makeLenses, view)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup (Semigroup)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.List.Group.WithScores
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Main Types
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


data GroupedTextParams a b =
  GroupedTextParams { _gt_fun_stem    :: Text -> Text
                    , _gt_fun_score   :: a -> b
                    , _gt_fun_texts   :: a -> Set Text
                    , _gt_fun_nodeIds :: a -> Set NodeId
                    -- , _gt_fun_size    :: a -> Int
                    }
makeLenses 'GroupedTextParams

type Stem  = Text
data GroupedText score =
  GroupedText { _gt_listType :: !(Maybe ListType)
              , _gt_label    :: !Text
              , _gt_score    :: !score
              , _gt_children :: !(Set Text)
              , _gt_size     :: !Int
              , _gt_stem     :: !Stem -- needed ?
              , _gt_nodes    :: !(Set NodeId)
              }  deriving Show --}
{-
instance Show score => Show (GroupedText score) where
  show (GroupedText lt l s _ _ _ _) = show l <> " : " <> show lt <> " : " <> show s
--}

instance (Eq a) => Eq (GroupedText a) where
  (==) (GroupedText _ _ score1 _ _ _ _)
       (GroupedText _ _ score2 _ _ _ _) = (==) score1 score2

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

------------------------------------------------------------------------
groupWithStem :: {- ( HasNgrams a
                 , HasGroupWithScores a b
                 , Semigroup a
                 ,  Ord b
                 ) 
              => -} GroupedTextParams a b
              -> Map Text (GroupedTextScores (Set NodeId))
              -> Map Stem (GroupedText Int)
groupWithStem _ = Map.mapWithKey scores2groupedText

scores2groupedText :: Text -> GroupedTextScores (Set NodeId) -> GroupedText Int
scores2groupedText t g = GroupedText (view gts_listType g)
                                     t
                                     (Set.size $ view gts_score g)
                                     (Set.delete t $ view gts_children g)
                                     (size t)
                                     t
                                     (view gts_score g)

------------------------------------------------------------------------
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
groupedTextWithStem :: Ord b
              => GroupedTextParams a b
              -> Map Text a
              -> Map Stem (GroupedText b)
groupedTextWithStem gparams from =
  Map.fromListWith (<>) $ map (group gparams) $ Map.toList from
    where
      group gparams' (t,d) = let t' = (view gt_fun_stem gparams') t
                     in (t', GroupedText
                                Nothing
                                t
                                ((view gt_fun_score gparams')   d)
                                ((view gt_fun_texts gparams')   d)
                                (size        t)
                                t'
                                ((view gt_fun_nodeIds gparams') d)
                         )
------------------------------------------------------------------------
