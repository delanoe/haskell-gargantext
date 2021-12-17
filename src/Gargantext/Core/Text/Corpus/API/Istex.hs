{-|
Module      : Gargantext.Core.Text.Corpus.API.Istex
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.Corpus.API.Istex
    where

import Data.List (concat)
import Data.Maybe
import Data.Text (Text, pack)

import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import qualified Gargantext.Defaults as Defaults
import Gargantext.Prelude
import qualified Gargantext.Core.Text.Corpus.Parsers.Date as Date
import qualified ISTEX        as ISTEX
import qualified ISTEX.Client as ISTEX

get :: Lang -> Text -> Maybe Integer -> IO [HyperdataDocument]
get la q ml = do
  --docs <- ISTEX.getMetadataWith q (fromIntegral <$> ml)
  printDebug "[Istex.get] calling getMetadataScrollWith for q" q
  printDebug "[Istex.get] calling getMetadataScrollWith for ml" ml
  -- The "scroll" expects "d/h/m/s/ms" time interval. Let's set it to "1 month"
  docs <- ISTEX.getMetadataScrollWith q ((\_n -> pack $ "1m") <$> ml) --(fromIntegral <$> ml)
  either (panic . pack . show) (toDoc' la) docs

toDoc' :: Lang -> ISTEX.Documents -> IO [HyperdataDocument]
toDoc' la docs' = do
  --printDebug "ISTEX" (ISTEX._documents_total docs')
  mapM (toDoc la) (ISTEX._documents_hits docs')

-- | TODO remove dateSplit here
-- TODO current year as default
toDoc :: Lang -> ISTEX.Document -> IO HyperdataDocument
toDoc la (ISTEX.Document i t a ab d s) = do
  (utctime, (pub_year, pub_month, pub_day)) <- Date.dateSplit la (maybe (Just $ pack $ show Defaults.year) (Just . pack . show) d)
  pure $ HyperdataDocument { _hd_bdd = Just "Istex"
                           , _hd_doi = Just i
                           , _hd_url = Nothing
                           , _hd_uniqId = Nothing
                           , _hd_uniqIdBdd = Nothing
                           , _hd_page = Nothing
                           , _hd_title = t
                           , _hd_authors = Just $ foldl (\x y -> x <> ", " <> y) "" (map ISTEX._author_name a)
                           , _hd_institutes = Just $ foldl (\x y -> x <> ", " <> y) "" (concat $ (map ISTEX._author_affiliations) a)
                           , _hd_source = Just $ foldl (\x y -> x <> ", " <> y) "" (catMaybes $ map ISTEX._source_title s)
                           , _hd_abstract = ab
                           , _hd_publication_date = fmap (pack . show) utctime
                           , _hd_publication_year = pub_year
                           , _hd_publication_month = pub_month
                           , _hd_publication_day = pub_day
                           , _hd_publication_hour = Nothing
                           , _hd_publication_minute = Nothing
                           , _hd_publication_second = Nothing
                           , _hd_language_iso2 = Just $ (pack . show) la }
                         
