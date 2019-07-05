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

module Gargantext.Text.Corpus.API
    where

--{-

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text)
import Gargantext.Prelude
--import qualified PUBMED as PubMed
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
import Data.Swagger
--import qualified Gargantext.Text.Corpus.API.Isidore as Isidore


data ExternalAPIs = ALL
                  | PubMed
                  | HAL
                  | IsTex
                  | IsidoreQuery | IsidoreAuth
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON ExternalAPIs
instance ToJSON ExternalAPIs
type Query = Text

externalAPIs :: [ExternalAPIs]
externalAPIs = [minBound..maxBound]

instance Arbitrary ExternalAPIs
  where
    arbitrary = elements externalAPIs

instance ToSchema ExternalAPIs
{-
crawl :: Crawler -> Query -> IO [PubMed.Doc]
crawl Pubmed = PubMed.crawler
--}
