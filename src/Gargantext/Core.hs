{-|
Module      : Gargantext.Core
Description : Supported Natural language
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core
  where

import Data.Text (Text)
import Data.Aeson
import Data.Either(Either(Left))
import Data.Hashable (Hashable)
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Prelude
import Servant.API

------------------------------------------------------------------------
-- | Language of a Text
-- For simplicity, we suppose text has an homogenous language
-- 
-- Next steps: | DE | IT | SP
--
--  - EN == english
--  - FR == french
--  - DE == deutch  (not implemented yet)
--  - IT == italian (not implemented yet)
--  - SP == spanish (not implemented yet)
--
--  ... add your language and help us to implement it (:

-- | All languages supported
-- TODO : DE | SP | CH
data Lang = EN | FR | All
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON Lang
instance FromJSON Lang
instance ToSchema Lang
instance FromHttpApiData Lang
  where
    parseUrlPiece "EN"  = pure EN
    parseUrlPiece "FR"  = pure FR
    parseUrlPiece "All" = pure All
    parseUrlPiece _     = Left "Unexpected value of OrderBy"
instance Hashable Lang

allLangs :: [Lang]
allLangs = [minBound ..]

class HasDBid a where
  toDBid   :: a   -> Int
  fromDBid :: Int -> a

instance HasDBid Lang where
  toDBid All = 0
  toDBid FR  = 1
  toDBid EN  = 2

  fromDBid 0 = All
  fromDBid 1 = FR
  fromDBid 2 = EN
  fromDBid _ = panic "HasDBid lang, not implemented"

------------------------------------------------------------------------
type Form = Text
type Lem  = Text
------------------------------------------------------------------------
data PosTagAlgo = CoreNLP
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable PosTagAlgo

instance HasDBid PosTagAlgo where
  toDBid CoreNLP = 1
  fromDBid 1 = CoreNLP
  fromDBid _ = panic "HasDBid posTagAlgo : Not implemented"

