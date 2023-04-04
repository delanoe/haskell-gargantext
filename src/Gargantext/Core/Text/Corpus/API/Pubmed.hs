{-|
Module      : Gargantext.Core.Text.Corpus.API.Pubmed
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.Corpus.API.Pubmed
    where

import Conduit
import Control.Monad.Reader (runReaderT)
import Data.Either (Either)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Client (ClientError)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

import qualified PUBMED as PubMed
import qualified PUBMED.Parser as PubMedDoc
import PUBMED.Types (Config(..))


type Query = Text
type Limit = Integer


-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Maybe Text
    -> Query
    -> Maybe Limit
    -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get mAPIKey q l = do
  eRes <- runReaderT PubMed.getMetadataWithC (Config { apiKey  = mAPIKey
                                                     , query   = q
                                                     , perPage = Just 200
                                                     , mWebEnv = Nothing })
  let takeLimit = case l of
        Nothing -> mapC identity
        Just l' -> takeC $ fromIntegral l'
  pure $ (\(len, docsC) -> (len, docsC .| takeLimit .| mapC (toDoc EN))) <$> eRes
  --either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN))
  --      <$> PubMed.getMetadataWithC q l

toDoc :: Lang -> PubMedDoc.PubMed -> HyperdataDocument
toDoc l (PubMedDoc.PubMed { pubmed_id
                          , pubmed_article = PubMedDoc.PubMedArticle t j as aus
                          , pubmed_date = PubMedDoc.PubMedDate a y m d }
          ) = HyperdataDocument { _hd_bdd = Just "PubMed"
                                , _hd_doi = Nothing
                                , _hd_url = Nothing
                                , _hd_uniqId = Just $ Text.pack $ show pubmed_id
                                , _hd_uniqIdBdd = Nothing
                                , _hd_page = Nothing
                                , _hd_title = t
                                , _hd_authors = authors aus
                                , _hd_institutes = institutes aus
                                , _hd_source = j
                                , _hd_abstract = abstract as
                                , _hd_publication_date = Just $ Text.pack $ show a
                                , _hd_publication_year = Just $ fromIntegral y
                                , _hd_publication_month = Just m
                                , _hd_publication_day = Just d
                                , _hd_publication_hour = Nothing
                                , _hd_publication_minute = Nothing
                                , _hd_publication_second = Nothing
                                , _hd_language_iso2 = Just $ (Text.pack . show) l }
      where
        authors :: [PubMedDoc.Author] -> Maybe Text
        authors [] = Nothing
        authors au = Just $ (Text.intercalate ", ")
                          $ catMaybes
                          $ map (\n -> PubMedDoc.foreName n <> Just " " <> PubMedDoc.lastName n) au

        institutes :: [PubMedDoc.Author] -> Maybe Text
        institutes [] = Nothing
        institutes au = Just $ (Text.intercalate ", ")
                             $ (map (Text.replace ", " " - "))
                             $ catMaybes
                             $ map PubMedDoc.affiliation au


        abstract :: [Text] -> Maybe Text
        abstract [] = Nothing
        abstract as' = Just $ Text.intercalate ", " as'
