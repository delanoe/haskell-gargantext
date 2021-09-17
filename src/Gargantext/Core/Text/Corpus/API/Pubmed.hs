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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

import qualified PUBMED as PubMed
import qualified PUBMED.Parser as PubMedDoc


type Query = Text
type Limit = PubMed.Limit


-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Query -> Maybe Limit -> IO [HyperdataDocument]
get q l = either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN))
        <$> PubMed.getMetadataWith q l

toDoc :: Lang -> PubMedDoc.PubMed -> HyperdataDocument
toDoc l (PubMedDoc.PubMed (PubMedDoc.PubMedArticle t j as aus)
                    (PubMedDoc.PubMedDate a y m d)
          ) = HyperdataDocument { _hd_bdd = Just "PubMed"
                                , _hd_doi = Nothing
                                , _hd_url = Nothing
                                , _hd_uniqId = Nothing
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
        authors :: Maybe [PubMedDoc.Author] -> Maybe Text
        authors aus' = case aus' of
            Nothing -> Nothing
            Just au -> Just $ (Text.intercalate ", ")
                            $ catMaybes
                            $ map (\n -> PubMedDoc.foreName n <> Just " " <> PubMedDoc.lastName n) au

        institutes :: Maybe [PubMedDoc.Author] -> Maybe Text
        institutes aus' = case aus' of
            Nothing -> Nothing
            Just au -> Just $ (Text.intercalate ", ")
                            $ (map (Text.replace ", " " - "))
                            $ catMaybes
                            $ map PubMedDoc.affiliation au


        abstract :: Maybe [Text] -> Maybe Text
        abstract as' = fmap (Text.intercalate ", ") as'

