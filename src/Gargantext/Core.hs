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

import Data.Aeson
import Data.Either(Either(Left))
import Data.Hashable (Hashable)
import Data.Morpheus.Types (GQLType)
import Data.Swagger
import Data.Text (Text, pack)
import Data.Tuple.Extra (swap)
import GHC.Generics (Generic)
import Gargantext.Prelude
import Servant.API
import qualified Data.Map as Map

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
--  - ZH == chinese
--
--  ... add your language and help us to implement it (:

-- | All languages supported
-- NOTE: Use international country codes
-- https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
data Lang = All
          | DE
          | EL
          | EN
          | ES
          | FR
          | IT
          | PL
          | PT
          | RU
          | UK
          | ZH
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, GQLType)

instance ToJSON Lang
instance FromJSON Lang
instance ToSchema Lang where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance FromHttpApiData Lang
  where
    parseUrlPiece "All" = pure All
    parseUrlPiece "DE"  = pure DE
    parseUrlPiece "EL"  = pure EL
    parseUrlPiece "EN"  = pure EN
    parseUrlPiece "ES"  = pure ES
    parseUrlPiece "FR"  = pure FR
    parseUrlPiece "IT"  = pure IT
    parseUrlPiece "PL"  = pure PL
    parseUrlPiece "PT"  = pure PT
    parseUrlPiece "RU"  = pure RU
    parseUrlPiece "UK"  = pure UK
    parseUrlPiece "ZH"  = pure ZH
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
dbIds :: [(Lang, Int)]
dbIds = [ (All, 0  )
        , (DE , 276)
        , (EL , 300)
        , (EN , 2  )
        , (ES , 724)
        , (FR , 1  )
        , (IT , 380)
        , (PL , 616)
        , (PT , 620)
        , (RU , 643)
        , (UK , 804)
        , (ZH , 156)
        ]

instance HasDBid Lang where
  toDBid lang = case Map.lookup lang $ Map.fromList dbIds of
                    Just la -> la
                    Nothing -> panic "[G.Core] Add this lang to DB ids"

  fromDBid dbId = case Map.lookup dbId $ Map.fromList $ map swap dbIds of
                    Just la -> la
                    Nothing -> panic "HasDBid lang, not implemented"

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
