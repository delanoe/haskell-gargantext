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

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Social.Patch
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Map.Strict.Patch as PatchMap
import qualified Data.Patch.Class as Patch (Replace(..))
import qualified Data.Set  as Set
import qualified Data.Text as Text

------------------------------------------------------------------------
addScoreStem :: GroupParams
             -> Set NgramsTerm
             -> FlowCont Text FlowListScores
             -> FlowCont Text FlowListScores
addScoreStem groupParams ngrams fl = foldl' addScorePatch fl 
                                   $ stemPatches groupParams ngrams

------------------------------------------------------------------------
-- | Main Types
data StopSize = StopSize {unStopSize :: !Int}
  deriving (Eq)

-- | TODO: group with 2 terms only can be
-- discussed. Main purpose of this is offering
-- a first grouping option to user and get some
-- enriched data to better learn and improve that algo
data GroupParams = GroupParams { unGroupParams_lang     :: !Lang
                               , unGroupParams_len      :: !Int
                               , unGroupParams_limit    :: !Int
                               , unGroupParams_stopSize :: !StopSize
                               }
                 | GroupIdentity
  deriving (Eq)

------------------------------------------------------------------------
groupWith :: GroupParams
            -> Text
            -> Text
groupWith GroupIdentity  = identity
groupWith (GroupParams l _m _n _) =
                    Text.intercalate " "
                  . map (stem l)
                  -- . take n
                  . List.sort
                  -- . (List.filter (\t -> Text.length t > m))
                  . Text.splitOn " "
                  . Text.replace "-" " "
--------------------------------------------------------------------
stemPatches :: GroupParams
           -> Set NgramsTerm
           -> [(NgramsTerm, NgramsPatch)]
stemPatches groupParams = patches
                        . Map.fromListWith (<>)
                        . map (\ng@(NgramsTerm t) -> ( groupWith groupParams t
                                                     , Set.singleton ng)
                              )
                        . Set.toList

-- | For now all NgramsTerm which have same stem
-- are grouped together
-- Parent is taken arbitrarly for now (TODO use a score like occ)
patches :: Map Stem (Set NgramsTerm)
            -> [(NgramsTerm, NgramsPatch)]
patches = catMaybes . map patch . Map.elems

patch :: Set NgramsTerm
           -> Maybe (NgramsTerm, NgramsPatch)
patch s = case Set.size s > 1 of
  False -> Nothing
  True  -> do
    let ngrams = Set.toList s
    parent   <- headMay ngrams
    let children = List.tail ngrams
    pure (parent, toNgramsPatch children)
    
toNgramsPatch :: [NgramsTerm] -> NgramsPatch
toNgramsPatch children = NgramsPatch children' Patch.Keep
  where
    children' :: PatchMSet NgramsTerm
    children' = PatchMSet
              $ fst
              $ PatchMap.fromList
              $ List.zip children (List.cycle [addPatch])

