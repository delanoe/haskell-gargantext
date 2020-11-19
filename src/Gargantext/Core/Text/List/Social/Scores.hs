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
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Prelude
import qualified Data.Map   as Map
import qualified Data.Set   as Set

------------------------------------------------------------------------
-- | Datatype definition
data FlowListScores =
  FlowListScores { _fls_parents  :: Map Parent   Int
                 , _fls_listType :: Map ListType Int
                -- You can add any score by incrementing this type
                -- , _flc_score   :: Map Score Int
                 }
    deriving (Show, Generic)

makeLenses ''FlowListScores

-- | Rules to compose 2 datatype FlowListScores
-- Triangle de Pascal, nombre d'or ou pi ?
instance Semigroup FlowListScores where
  (<>) (FlowListScores p1 l1)
       (FlowListScores p2 l2) =
        FlowListScores (p1 <> p2)
                       (l1 <> l2)

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Generates Score from list of Map Text NgramsRepoElement
toFlowListScores :: KeepAllParents
                 -> Set Text
                 ->  Map Text FlowListScores
                 -> [Map Text NgramsRepoElement]
                 ->  Map Text FlowListScores
toFlowListScores k st = foldl' (toFlowListScores' k st)
  where
    toFlowListScores' :: KeepAllParents
                     -> Set Text
                     -> Map Text FlowListScores
                     -> Map Text NgramsRepoElement
                     -> Map Text FlowListScores
    toFlowListScores' k' st' to' ngramsRepo =
      Set.foldl' (toFlowListScores'' k' st' ngramsRepo) to' st'

    toFlowListScores'' :: KeepAllParents
                       -> Set Text
                       -> Map Text NgramsRepoElement
                       -> Map Text FlowListScores
                       -> Text
                       -> Map Text FlowListScores
    toFlowListScores'' k'' st'' ngramsRepo to'' t =
      case Map.lookup t ngramsRepo of
        Nothing  -> to''
        Just nre -> Map.alter (addParent k'' nre st'')        t
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
