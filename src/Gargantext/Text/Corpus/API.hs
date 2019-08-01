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
import Data.Maybe
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
import Data.Swagger
import qualified Data.Text as Text

import qualified PUBMED as PubMed
import qualified PUBMED.Parser as Doc
--import qualified Gargantext.Text.Corpus.API.Isidore as Isidore

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

get :: ExternalAPIs -> Query -> Maybe Limit -> IO [HyperdataDocument]
get PubMed q l = either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN)) <$> PubMed.crawler q l
get _ _ _ = undefined

toDoc :: Lang -> Doc.PubMed -> HyperdataDocument
toDoc l (Doc.PubMed (Doc.PubMedArticle t j as aus)
                    (Doc.PubMedDate a y m d)
          ) = HyperdataDocument (Just "PubMed")
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 t
                                 (authors aus)
                                 Nothing
                                 j
                                 (abstract as)
                                 (Just $ Text.pack $ show a)
                                 (Just $ fromIntegral y)
                                 (Just m)
                                 (Just d)
                                 Nothing
                                 Nothing
                                 Nothing
                                 (Just $ (Text.pack . show) l)
      where
        authors :: Maybe [Doc.Author] -> Maybe Text
        authors aus' = case aus' of
            Nothing -> Nothing
            Just au -> Just $ (Text.intercalate ", ") $ catMaybes $ map Doc.foreName au

        abstract :: Maybe [Text] -> Maybe Text
        abstract as' = fmap (Text.intercalate ", ") as'

