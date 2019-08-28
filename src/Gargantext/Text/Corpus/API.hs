{-|
Module      : Gargantext.Text.Corpus.API
Description : All crawlers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}

module Gargantext.Text.Corpus.API
    where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Maybe
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
import Data.Swagger


import qualified Gargantext.Text.Corpus.API.Pubmed  as PUBMED
import qualified Gargantext.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Text.Corpus.API.Istex   as ISTEX

-- | Main Types
data ExternalAPIs = All
                  | PubMed

                  | Hal_EN
                  | Hal_FR

                  | IsTex_EN
                  | IsTex_FR

                  | Isidore_EN
                  | Isidore_FR
                  -- | IsidoreAuth
  deriving (Show, Eq, Enum, Bounded, Generic)


-- | Get External API metadata main function
get :: ExternalAPIs -> Query -> Maybe Limit -> IO [HyperdataDocument]
get All        _ _ = undefined

get PubMed     q l = PUBMED.get q l

get Hal_EN     q l = HAL.get EN q l
get Hal_FR     q l = HAL.get FR q l

get IsTex_EN   q l = ISTEX.get EN q l
get IsTex_FR   q l = ISTEX.get FR q l

get Isidore_EN q l = ISIDORE.get EN (fromIntegral <$> l) (Just q) Nothing
get Isidore_FR q l = ISIDORE.get FR (fromIntegral <$> l) (Just q) Nothing


-- | Main Instances
instance FromJSON ExternalAPIs
instance ToJSON ExternalAPIs

externalAPIs :: [ExternalAPIs]
externalAPIs = [minBound..maxBound]

instance Arbitrary ExternalAPIs
  where
    arbitrary = elements externalAPIs

instance ToSchema ExternalAPIs

-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit

