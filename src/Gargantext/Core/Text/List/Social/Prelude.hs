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
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Monoid
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Prelude
import qualified Gargantext.Data.HashMap.Strict.Utils as HashMap
import qualified Data.Map.Strict       as Map
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Map.Strict.Patch as PatchMap

------------------------------------------------------------------------
type Parent = NgramsTerm
------------------------------------------------------------------------
-- | DataType inspired by continuation Monad (but simpler)
data FlowCont a b =
  FlowCont { _flc_scores :: HashMap a b
           , _flc_cont   :: HashMap a b
           }
    deriving (Show)

instance (Ord a, Eq b, Hashable a) => Monoid (FlowCont a b) where
  mempty = FlowCont mempty mempty

instance (Eq a, Ord a, Eq b, Hashable a) => Semigroup (FlowCont a b) where
  (<>) (FlowCont  m1    s1)
       (FlowCont  m2    s2)
      = FlowCont (m1 <> m2)
                 (s1 <> s2)

makeLenses ''FlowCont

-- | Datatype definition
data FlowListScores =
  FlowListScores { _fls_listType :: HashMap ListType Int
                 , _fls_parents  :: HashMap Parent   Int
                -- You can add any score by incrementing this type
                -- , _flc_score   :: HashMap Score Int
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
  mempty = FlowListScores HashMap.empty HashMap.empty

------------------------------------------------------------------------
-- | Tools to inherit groupings
------------------------------------------------------------------------
-- | Tools
parentUnionsMerge :: (Ord a, Ord b, Num c, Hashable a, Hashable b)
                   => [HashMap a (HashMap b c)]
                   ->  HashMap a (HashMap b c)
parentUnionsMerge = HashMap.unionsWith (HashMap.unionWith (+))

-- This Parent union is specific
-- [Private, Shared, Public]
-- means the following preferences:
-- Private > Shared > Public
-- if data have not been tagged privately, then use others tags
-- This unions behavior takes first key only and ignore others
parentUnionsExcl :: (Ord a, Hashable a)
                 => [HashMap a b]
                 ->  HashMap a b
parentUnionsExcl = HashMap.unions

------------------------------------------------------------------------
-- | Takes key with max value if and only if value > 0
-- If value <= 0 alors key is not taken at all
-- It can happens since some score are non positive (i.e. removing a child)
-- >>> keyWithMaxValue $ DM.fromList $ zip (['a'..'z'] :: [Char]) ([1,2..]::[Int])
-- Just 'z'
-- >>> keyWithMaxValue $ DM.fromList $ zip (['a'..'z'] :: [Char]) ([-1,-2..]::[Int])
-- Nothing
-- TODO duplicate with getMaxFromMap and improve it (lookup value should not be needed)
-- TODO put in custom Prelude
keyWithMaxValue :: (Ord a, Ord b, Num b, Hashable a)
                => HashMap a b -> Maybe a
keyWithMaxValue m = do
  maxKey   <- headMay $ HashMap.getKeysOrderedByValueMaxFirst m
  maxValue <- HashMap.lookup maxKey m
  if maxValue > 0
     then pure maxKey
     else Nothing


------------------------------------------------------------------------
unPatchMapToHashMap :: (Ord a, Hashable a) => PatchMap a b -> HashMap a b
unPatchMapToHashMap = HashMap.fromList . PatchMap.toList

unPatchMapToMap :: Ord a => PatchMap a b -> Map a b
unPatchMapToMap = Map.fromList . PatchMap.toList

unNgramsTablePatch :: NgramsTablePatch -> HashMap NgramsTerm NgramsPatch
unNgramsTablePatch (NgramsTablePatch p) = unPatchMapToHashMap p

