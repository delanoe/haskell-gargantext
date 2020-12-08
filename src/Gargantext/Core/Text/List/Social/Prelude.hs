{-|
Module      : Gargantext.Core.Text.List.Social.Prelude
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
------------------------------------------------------------------------
module Gargantext.Core.Text.List.Social.Prelude
  where

import Control.Lens
import Data.Semigroup (Semigroup(..))
import Data.Monoid
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core.Types.Main
import Gargantext.Prelude
import GHC.Generics (Generic)
import qualified Data.Map   as Map

------------------------------------------------------------------------
type Parent = Text
------------------------------------------------------------------------
-- | DataType inspired by continuation Monad (but simpler)
data FlowCont a b =
  FlowCont { _flc_scores :: Map a b
           , _flc_cont   :: Map a b
           }
    deriving (Show)

instance (Ord a, Eq b) => Monoid (FlowCont a b) where
  mempty = FlowCont mempty mempty

instance (Eq a, Ord a, Eq b) => Semigroup (FlowCont a b) where
  (<>) (FlowCont  m1    s1)
       (FlowCont  m2    s2)
      = FlowCont (m1 <> m2)
                 (s1 <> s2)

makeLenses ''FlowCont

-- | Datatype definition
data FlowListScores =
  FlowListScores { _fls_listType :: Map ListType Int
                 , _fls_parents  :: Map Parent   Int
                -- You can add any score by incrementing this type
                -- , _flc_score   :: Map Score Int
                 }
    deriving (Show, Generic, Eq)

makeLenses ''FlowListScores

-- | Rules to compose 2 datatype FlowListScores
-- About the shape of the Type fun:
-- Triangle de Pascal, nombre d'or ou pi ?
-- Question: how to add a score field and derive such definition
-- without the need to fix it below ?
instance Semigroup FlowListScores where
  (<>) (FlowListScores p1 l1)
       (FlowListScores p2 l2) =
        FlowListScores (p1 <> p2)
                       (l1 <> l2)

instance Monoid FlowListScores where
  mempty = FlowListScores Map.empty Map.empty

------------------------------------------------------------------------
-- | Tools to inherit groupings
------------------------------------------------------------------------
-- | Tools
parentUnionsMerge :: (Ord a, Ord b, Num c)
                   => [Map a (Map b c)]
                   ->  Map a (Map b c)
parentUnionsMerge = Map.unionsWith (Map.unionWith (+))

-- This Parent union is specific
-- [Private, Shared, Public]
-- means the following preferences:
-- Private > Shared > Public
-- if data have not been tagged privately, then use others tags
-- This unions behavior takes first key only and ignore others
parentUnionsExcl :: Ord a
                 => [Map a b]
                 ->  Map a b
parentUnionsExcl = Map.unions


------------------------------------------------------------------------

hasParent :: Text
          -> Map Text (Map Parent Int)
          -> Maybe Parent
hasParent t m = case Map.lookup t m of
  Nothing  -> Nothing
  Just  m' -> keyWithMaxValue m'

------------------------------------------------------------------------
keyWithMaxValue :: Map a b -> Maybe a
keyWithMaxValue m = (fst . fst) <$> Map.maxViewWithKey m
