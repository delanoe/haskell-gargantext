{-|
Module      : Gargantext.Core.Text.List.Social.Group
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


module Gargantext.Core.Text.List.Social.Group
  where

import Control.Lens
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Individu
import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
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
  Just  m' -> (fst . fst) <$> Map.maxViewWithKey m'

------------------------------------------------------------------------

data FlowListScores =
  FlowListScores { _flc_parents :: Map Parent   Int
                 , _flc_lists   :: Map ListType Int
                -- You can add any score by incrementing this type
                -- , _flc_score   :: Map Score Int
                 }
    deriving (Generic)

makeLenses ''FlowListScores

------------------------------------------------------------------------
-- | toFlowListScores which generate Score from list of Map Text NgramsRepoElement

toFlowListScores :: Set Text
                -> Map Text FlowListScores
                -> [Map Text NgramsRepoElement]
                -> Map Text FlowListScores
toFlowListScores ts = foldl' (toFlowListScores' ts)
  where
    toFlowListScores' :: Set Text
                     -> Map Text FlowListScores
                     -> Map Text NgramsRepoElement
                     -> Map Text FlowListScores
    toFlowListScores' ts' to' ngramsRepo =
      Set.foldl' (toFlowListScores'' ts' ngramsRepo) to' ts'

    toFlowListScores'' :: Set Text
                      -> Map Text NgramsRepoElement
                      -> Map Text FlowListScores
                      -> Text
                      -> Map Text FlowListScores
    toFlowListScores'' ss ngramsRepo to'' t =
      case Map.lookup t ngramsRepo of
        Nothing  -> to''
        Just nre -> Map.alter (addParent nre ss)        t
                  $ Map.alter (addList $ _nre_list nre) t to''

------------------------------------------------------------------------
-- | Main addFunctions to FlowListScores
------------------------------------------------------------------------

-- | Very unseful but nice comment:
-- "this function looks like an ASCII bird"
addList :: ListType
        -> Maybe FlowListScores
        -> Maybe FlowListScores
addList l Nothing =
  Just $ FlowListScores Map.empty (addList' l Map.empty)

addList l (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent mapList'
    where
      mapList' = addList' l mapList


addList' :: ListType -> Map ListType Int -> Map ListType Int
addList' l m = Map.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus _ Nothing              = Just 3
    plus _ (Just x)             = Just $ x + 3

------------------------------------------------------------------------
------------------------------------------------------------------------
addParent :: NgramsRepoElement -> Set Text
          -> Maybe FlowListScores
          -> Maybe FlowListScores

addParent nre ss Nothing  =
  Just $ FlowListScores mapParent Map.empty
    where
      mapParent = addParent' (_nre_parent nre) ss Map.empty

addParent nre ss (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent' mapList
    where
      mapParent' = addParent' (_nre_parent nre) ss mapParent

addParent' :: Num a
           => Maybe NgramsTerm
           -> Set Text
           -> Map Text a
           -> Map Text a
addParent' Nothing               _ss mapParent = mapParent
addParent' (Just (NgramsTerm p')) ss mapParent =
  if not (Set.member p' ss)
    then mapParent
    else Map.alter addCount p' mapParent
      where
        addCount Nothing  = Just 1
        addCount (Just n) = Just $ n + 1

------------------------------------------------------------------------
