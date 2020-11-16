{-|
Module      : Gargantext.Core.Text.List.Social.Scores
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}


module Gargantext.Core.Text.List.Social.Scores
  where

import Control.Lens
import Data.Map (Map)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Prelude
import qualified Data.Map   as Map
import qualified Data.Set   as Set

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
type Parent = Text

hasParent :: Text
          -> Map Text (Map Parent Int)
          -> Maybe Parent
hasParent t m = case Map.lookup t m of
  Nothing  -> Nothing
  Just  m' -> keyWithMaxValue m'

------------------------------------------------------------------------
keyWithMaxValue :: Map a b -> Maybe a
keyWithMaxValue m = (fst . fst) <$> Map.maxViewWithKey m

------------------------------------------------------------------------
data FlowListScores =
  FlowListScores { _flc_parents :: Map Parent   Int
                 , _flc_lists   :: Map ListType Int
                -- You can add any score by incrementing this type
                -- , _flc_score   :: Map Score Int
                 }
    deriving (Generic)

makeLenses ''FlowListScores

instance Semigroup FlowListScores where
  (<>) (FlowListScores p1 l1) (FlowListScores p2 l2) =
    FlowListScores (p1 <> p2) (l1 <> l2)


------------------------------------------------------------------------
------------------------------------------------------------------------
-- | toFlowListScores which generate Score from list of Map Text
--   NgramsRepoElement
toFlowListScores :: KeepAllParents
                 -> Set Text
                 ->  Map Text FlowListScores
                 -> [Map Text NgramsRepoElement]
                 ->  Map Text FlowListScores
toFlowListScores k ts = foldl' (toFlowListScores' k ts)
  where
    toFlowListScores' :: KeepAllParents
                     -> Set Text
                     -> Map Text FlowListScores
                     -> Map Text NgramsRepoElement
                     -> Map Text FlowListScores
    toFlowListScores' k' ts' to' ngramsRepo =
      Set.foldl' (toFlowListScores'' k' ts' ngramsRepo) to' ts'

    toFlowListScores'' :: KeepAllParents
                       -> Set Text
                       -> Map Text NgramsRepoElement
                       -> Map Text FlowListScores
                       -> Text
                       -> Map Text FlowListScores
    toFlowListScores'' k'' ss ngramsRepo to'' t =
      case Map.lookup t ngramsRepo of
        Nothing  -> to''
        Just nre -> Map.alter (addParent k'' nre ss)        t
                  $ Map.alter (addList $ _nre_list nre) t to''

------------------------------------------------------------------------
-- | Main addFunctions to groupResolution the FlowListScores
-- Use patch-map library here
-- diff, transformWith patches simplifies functions below
addList :: ListType
        -> Maybe FlowListScores
        -> Maybe FlowListScores
addList l Nothing =
  Just $ FlowListScores Map.empty (addList' l Map.empty)

addList l (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent mapList'
    where
      mapList' = addList' l mapList
-- * Unseful but nice comment:
-- "the addList function looks like an ASCII bird"

-- | Concrete function to pass to PatchMap
addList' :: ListType -> Map ListType Int -> Map ListType Int
addList' l m = Map.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus MapTerm Nothing        = Just 2
    plus MapTerm (Just x)       = Just $ x + 2

    plus StopTerm Nothing       = Just 3
    plus StopTerm (Just x)      = Just $ x + 3

------------------------------------------------------------------------
------------------------------------------------------------------------
data KeepAllParents = KeepAllParents Bool

addParent :: KeepAllParents -> NgramsRepoElement -> Set Text
          -> Maybe FlowListScores
          -> Maybe FlowListScores

addParent k nre ss Nothing  =
  Just $ FlowListScores mapParent Map.empty
    where
      mapParent = addParent' k (_nre_parent nre) ss Map.empty

addParent k nre ss (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent' mapList
    where
      mapParent' = addParent' k (_nre_parent nre) ss mapParent

addParent' :: Num a
           => KeepAllParents
           -> Maybe NgramsTerm
           -> Set Text
           -> Map Text a
           -> Map Text a
addParent' _ Nothing               _ss mapParent = mapParent
addParent' (KeepAllParents k) (Just (NgramsTerm p')) ss mapParent =
  case k of
    True  -> Map.alter addCount p' mapParent
    False -> case Set.member p' ss of
               False -> mapParent
               True  -> Map.alter addCount p' mapParent
  where
        addCount Nothing  = Just 1
        addCount (Just n) = Just $ n + 1

------------------------------------------------------------------------
