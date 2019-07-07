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

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Flow (FlowCorpus)
import Gargantext.Database.Types.Node (HyperdataDocument)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
import Data.Swagger

import qualified PUBMED as PubMed
import qualified PUBMED.Parser as Doc (PubMed)
import qualified Gargantext.Text.Corpus.API.Isidore as Isidore

data ExternalAPIs = All
                  | PubMed
                  | HAL
                  -- | IsTex
                  | IsidoreQuery | IsidoreAuth
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON ExternalAPIs
instance ToJSON ExternalAPIs

externalAPIs :: [ExternalAPIs]
externalAPIs = [minBound..maxBound]

instance Arbitrary ExternalAPIs
  where
    arbitrary = elements externalAPIs

instance ToSchema ExternalAPIs

type Query = Text
type Limit = PubMed.Limit

get :: FlowCorpus a => ExternalAPIs -> Query -> Maybe Limit -> IO [a]
get PubMed q l = either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN)) <$> PubMed.crawler q l
get _ _ _ = undefined

toDoc :: FlowCorpus a => Lang -> Doc.PubMed -> a
toDoc = undefined

