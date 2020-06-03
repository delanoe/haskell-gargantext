{-|
Module      : Gargantext.Text.Terms.Multi.Lang.En
Description : English Grammar rules to group postag tokens.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Rule-based grammars are computed in this english module in order to group
the tokens into extracted terms.

-}


module Gargantext.Text.Terms.Multi.Lang.En (group)
  where

import Gargantext.Prelude
import Gargantext.Core.Types
import Gargantext.Text.Terms.Multi.Group

------------------------------------------------------------------------
-- | Rule grammar to group tokens
group :: [TokenTag] -> [TokenTag]
group [] = []
group ntags = group2 NP NP
           $ group2 NP VB
--           $ group2 NP IN
           $ group2 IN DT
--           $ group2 VB NP
           $ group2 JJ NP
           $ group2 JJ JJ
           $ group2 JJ CC
           $ ntags

------------------------------------------------------------------------
--groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
--groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)
--groupNgrams ((x,_,"LOCATION"):(y,yy,"LOCATION"):xs)         = groupNgrams ((x <> " " <> y,yy,"LOCATION"):xs)
--
--groupNgrams (x:xs)                                          = (x:(groupNgrams xs))

