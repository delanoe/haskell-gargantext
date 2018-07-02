{-|
Module      : Gargantext.Text.List.Types
Description : 
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.List.Types where

import Prelude (Bounded, Enum, minBound, maxBound)
import Data.Text (Text)
import Data.Map (Map, empty, fromList)
import Gargantext.Prelude

-------------------------------------------------------------------
type Label = Text

data ListType = Map | Stop | Candidate
  deriving (Show, Eq, Ord, Enum, Bounded)

type Lists = Map ListType (Map Text [Text])


emptyLists :: Lists
emptyLists = fromList $ map (\lt -> (lt, empty))
                            ([minBound..maxBound] :: [ListType])

