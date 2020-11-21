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

import Control.Lens (set)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.List.Social.Prelude (FlowListScores(..))
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Group.WithScores
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

------------------------------------------------------------------------
toGroupedText :: GroupedTextParams a b
              -> Map Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Stem (GroupedText Int)
toGroupedText groupParams scores =
  (groupWithStem groupParams) . (groupWithScores scores)

------------------------------------------------------------------------
-- | TODO put in test folder
toGroupedText_test :: Bool -- Map Stem (GroupedText Int)
toGroupedText_test =
  -- fromGroupedScores $ fromListScores from
  toGroupedText params from datas == result
    where
      params = GroupedTextParams identity (Set.size . snd) fst snd
      from :: Map Text FlowListScores
      from = Map.fromList [("A. Rahmani",FlowListScores {_fls_parents = Map.fromList [("T. Reposeur",1)]
                                                        ,_fls_listType = Map.fromList [(MapTerm,2)]})
                           ,("B. Tamain",FlowListScores {_fls_parents = Map.fromList [("T. Reposeur",1)]
                                                        , _fls_listType = Map.fromList [(MapTerm,2)]})
                           ]

      datas :: Map Text (Set NodeId)
      datas = Map.fromList [("A. Rahmani" , Set.fromList [1,2])
                           ,("T. Reposeur", Set.fromList [3,4])
                           ,("B. Tamain"  , Set.fromList [5,6])
                           ]


      result :: Map Stem (GroupedText Int)
      result = Map.fromList [("A. Rahmani",GroupedText {_gt_listType = Nothing
                                                       ,_gt_label = "A. Rahmani"
                                                       ,_gt_score = 2
                                                       ,_gt_children = Set.empty
                                                       ,_gt_size = 2
                                                       ,_gt_stem = "A. Rahmani"
                                                       ,_gt_nodes = Set.fromList [1,2]
                                                       }
                              )
                            ,("B. Tamain",GroupedText {_gt_listType = Nothing
                                                      , _gt_label = "B. Tamain"
                                                      , _gt_score = 2
                                                      , _gt_children = Set.empty
                                                      , _gt_size = 2
                                                      , _gt_stem = "B. Tamain"
                                                      , _gt_nodes = Set.fromList [5,6]
                                                      }
                              )
                            ,("T. Reposeur",GroupedText {_gt_listType = Nothing
                                                        ,_gt_label = "T. Reposeur"
                                                        ,_gt_score = 2
                                                        ,_gt_children = Set.fromList ["A. Rahmani","B. Tamain"]
                                                        ,_gt_size = 2
                                                        ,_gt_stem = "T. Reposeur"
                                                        ,_gt_nodes = Set.fromList [1..6]
                                                        }
                            )
                           ]

------------------------------------------------------------------------
-- | TODO To be removed
addListType :: Map Text ListType -> GroupedText a -> GroupedText a
addListType m g = set gt_listType (hasListType m g) g
  where
    hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
    hasListType m' (GroupedText _ label _ g' _ _ _) =
        List.foldl' (<>) Nothing
      $ map (\t -> Map.lookup t m')
      $ Set.toList
      $ Set.insert label g'
