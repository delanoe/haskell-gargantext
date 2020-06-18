{-|
Module      : Gargantext.Text.Terms.Multi.Group
Description : English Grammar rules to group postag tokens.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Rule-based grammars are computed in this english module in order to
group the tokens into extracted terms.

-}


module Gargantext.Text.Terms.Multi.Group (group2)
  where

import Data.Maybe (Maybe(Just))

import Gargantext.Core.Types
import Gargantext.Prelude

-- | FIXME p1 and p2 not really taken into account
group2 :: POS -> POS -> [TokenTag] -> [TokenTag]
group2 p1 p2 (x@(TokenTag _ _ (Just p1') _):y@(TokenTag _ _ (Just p2') _):z) =
  if (p1 == p1') && (p2 == p2')
     then group2 p1 p2 (x<>y : z)
     else (x : group2 p1 p2 (y:z))
group2 p1 p2 (x@(TokenTag _ _ Nothing _):y) = (x: group2 p1 p2 y)
group2 _ _ [x@(TokenTag _ _ (Just _) _)] = [x]
group2 p1 p2 (x@(TokenTag _ _ (Just _) _):y@(TokenTag _ _ Nothing _):z) = (x:y: group2 p1 p2 (y:z))
group2 _ _ [] = []



