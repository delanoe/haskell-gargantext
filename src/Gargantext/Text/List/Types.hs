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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.List.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map, empty, fromList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Prelude
import Prelude (Bounded, Enum, minBound, maxBound)
import Data.Swagger (ToSchema)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


-------------------------------------------------------------------
data ListType = GraphList | StopList | CandidateList
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON ListType
instance ToJSON   ListType
instance ToSchema ListType
instance Arbitrary ListType where
  arbitrary = elements [minBound..maxBound]

type Lists = Map ListType (Map Text [Text])

type ListId = Int
type ListTypeId = Int

listTypeId :: ListType -> ListTypeId
listTypeId GraphList     = 1
listTypeId StopList      = 2
listTypeId CandidateList = 3


emptyLists :: Lists
emptyLists = fromList $ map (\lt -> (lt, empty))
                            ([minBound..maxBound] :: [ListType])

