{-|
Module      : Gargantext.Core
Description : Supported Natural language
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveAnyClass    #-}

module Gargantext.Core
  where

import Data.Text (Text, pack)
import Data.Aeson
import Data.Either(Either(Left))
import Data.Hashable (Hashable)
import Data.Morpheus.Types (GQLType)
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Prelude
import Servant.API

------------------------------------------------------------------------
-- | Language of a Text
-- For simplicity, we suppose text has an homogenous language
--
--  - EN == english
--  - FR == french
--  - DE == deutch
--  - IT == italian
--  - ES == spanish
--  - PL == polish
--  - CN == chinese
--
--  ... add your language and help us to implement it (:

-- | All languages supported
-- NOTE: Use international country codes
-- https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
data Lang = EN | FR | DE | IT | PL | ES | CN | All
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, GQLType)

instance ToJSON Lang
instance FromJSON Lang
instance ToSchema Lang where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance FromHttpApiData Lang
  where
    parseUrlPiece "EN"  = pure EN
    parseUrlPiece "FR"  = pure FR
    parseUrlPiece "DE"  = pure DE
    parseUrlPiece "ES"  = pure ES
    parseUrlPiece "IT"  = pure IT
    parseUrlPiece "PL"  = pure PL
    parseUrlPiece "CN"  = pure CN
    parseUrlPiece "All" = pure All
    parseUrlPiece _     = Left "Unexpected value of Lang"
instance ToHttpApiData Lang where
  toUrlPiece = pack . show
instance Hashable Lang

allLangs :: [Lang]
allLangs = [minBound ..]

class HasDBid a where
  toDBid   :: a   -> Int
  fromDBid :: Int -> a

-- NOTE: We try to use numeric codes for countries
-- https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
-- https://en.wikipedia.org/wiki/ISO_3166-1_numeric#004
instance HasDBid Lang where
  toDBid All = 0
  toDBid FR  = 1
  toDBid EN  = 2
  toDBid DE  = 276
  toDBid ES  = 724
  toDBid IT  = 380
  toDBid PL  = 616
  toDBid CN  = 156

  fromDBid 0 = All
  fromDBid 1 = FR
  fromDBid 2 = EN
  fromDBid 276 = DE
  fromDBid 724 = ES
  fromDBid 380 = IT
  fromDBid 616 = PL
  fromDBid 156 = CN
  fromDBid _ = panic "HasDBid lang, not implemented"

------------------------------------------------------------------------
data NLPServerConfig = NLPServerConfig
  { server :: !PosTagAlgo
  , url    :: !URI }
  deriving (Show, Eq, Generic)
------------------------------------------------------------------------
type Form = Text
type Lem  = Text
------------------------------------------------------------------------
data PosTagAlgo = CoreNLP | JohnSnowServer | Spacy
  deriving (Show, Read, Eq, Ord, Generic, GQLType)

instance Hashable PosTagAlgo

instance HasDBid PosTagAlgo where
  toDBid CoreNLP        = 1
  toDBid JohnSnowServer = 2
  toDBid Spacy          = 3
  fromDBid 1 = CoreNLP
  fromDBid 2 = JohnSnowServer
  fromDBid 3 = Spacy
  fromDBid _ = panic "HasDBid posTagAlgo : Not implemented"
