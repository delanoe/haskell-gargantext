{-|
Module      : Gargantext.Core.Text.Terms.Multi.Lang.Fr
Description : French Grammar rules to group postag tokens.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This @group@ function groups horizontally ngrams in their context of
sentence according to grammars specific of each language. In english, JJ
is ADJectiv in french.

-}


module Gargantext.Core.Text.Terms.Multi.Lang.Fr (groupTokens)
  where

import Gargantext.Prelude
import Gargantext.Core.Types
import Gargantext.Core.Text.Terms.Multi.Group (group2)

groupTokens :: [TokenTag] -> [TokenTag]
groupTokens [] = []
groupTokens ntags = group2 NP NP
            $ group2 NP VB
            -- group2 NP IN
            -- group2 IN DT
            $ group2 VB NP
            $ group2 JJ NP
            $ group2 NP JJ
            $ group2 JJ JJ
            -- group2 JJ CC
            $ ntags

------------------------------------------------------------------------
-- TODO
--groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
--groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)
