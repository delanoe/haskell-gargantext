{-|
Module      : Gargantext.Core
Description : Supported Natural language
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.Core
  where

import Gargantext.Prelude
import GHC.Generics (Generic)
import Data.Aeson
import Data.Either(Either(Left))
import Data.Swagger
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
    parseUrlPiece "EN" = pure EN
    parseUrlPiece "FR" = pure FR
    parseUrlPiece "All" = pure All
    parseUrlPiece _            = Left "Unexpected value of OrderBy"
allLangs :: [Lang]
allLangs = [minBound ..]
