{-|
Module      : Gargantext.Core.Text.List.Social.Prelude
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


module Gargantext.Core.Text.List.Social.Prelude
  where

import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Prelude
import qualified Data.Map   as Map

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
