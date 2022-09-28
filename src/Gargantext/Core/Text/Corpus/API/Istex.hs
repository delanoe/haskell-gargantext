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

import Data.Either (Either(..))
import Data.List (concat)
import Data.Maybe
import Data.Text (Text, pack)

import qualified Data.Text as Text
import qualified Data.List as List
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import qualified Gargantext.Defaults as Defaults
import Gargantext.Prelude
import qualified Gargantext.Core.Text.Corpus.Parsers.Date as Date
import qualified ISTEX        as ISTEX
import qualified ISTEX.Client as ISTEX

type Query = Text
type MaxResults = Maybe Integer

get :: Lang -> Query -> MaxResults -> IO [HyperdataDocument]
get la query' maxResults = do
  --printDebug "[Istex.get] calling getMetadataScrollProgress for la" la
  --printDebug "[Istex.get] calling getMetadataScrollProgress for q" q
  --printDebug "[Istex.get] calling getMetadataScrollProgress for ml" ml
  -- The "scroll" expects "d/h/m/s/ms" time interval. Let's set it to "1 month"
  --eDocs <- ISTEX.getMetadataScroll q ((\_n -> pack $ "1m") <$> ml) Nothing 0  --(fromIntegral <$> ml)

  -- TODO check if abstract is in query already if not add like below
  -- eDocs <- ISTEX.getMetadataScroll (q <> " abstract:*")  "1m" Nothing 0  --(fromIntegral <$> ml)
  -- eDocs <- ISTEX.getMetadataScroll q "1m" Nothing 0  --(fromIntegral <$> ml)

  let query = case (List.length $ Text.splitOn ":" query') == 1 of
        -- True case means users is entering default search of IsTex
        -- In that case we need to enrich his query with 2 parameters
        -- First expected language: user has to define it in GTXT
        -- Second : query in abstract
        True  -> ("language:"<> lang la) <> " AND abstract:"<>query'
            where
              lang FR = "fre"
              lang _  = "eng"

        False -> query'
        -- Complex queries of IsTex needs parameters using ":" so we leave the query as it is
        -- in that case we suppose user is knowing what s.he is doing

  eDocs <- ISTEX.getMetadataWith query (fromIntegral <$> maxResults)
  printDebug "[Istex.get] will print length" (0 :: Int)
  case eDocs of
    Left _ -> pure ()
    Right (ISTEX.Documents { _documents_hits }) -> printDebug "[Istex.get] length docs" $ length _documents_hits
  --ISTEX.getMetadataScrollProgress q ((\_ -> pack $ "1m") <$> ml) Nothing progress errorHandler
  case eDocs of
    Left err -> panic . Text.pack . show $ err
    Right docs -> toDoc' la docs
  --pure $ either (panic . pack . show) (toDoc' la) eDocs
--  where
--    progress (ISTEX.ScrollResponse { _scroll_documents = ISTEX.Documents { _documents_hits }}) =
--      printDebug "[Istex.get] got docs: " $ length _documents_hits
--    errorHandler err = printDebug "[Istex.get] error" $ show err

toDoc' :: Lang -> ISTEX.Documents -> IO [HyperdataDocument]
toDoc' la docs' =  mapM (toDoc la) (ISTEX._documents_hits docs')
  --printDebug "ISTEX" (ISTEX._documents_total docs')

-- | TODO remove dateSplit here
-- TODO current year as default
toDoc :: Lang -> ISTEX.Document -> IO HyperdataDocument
toDoc la (ISTEX.Document i t a ab d s) = do
  --printDebug "ISTEX date" d
  (utctime, (pub_year, pub_month, pub_day)) <-
        Date.dateSplit la (maybe (Just $ pack $ show Defaults.year) (Just . pack . show) d)
  --printDebug "toDoc Istex" (utctime, (pub_year, pub_month, pub_day))
  pure $ HyperdataDocument { _hd_bdd       = Just "Istex"
                           , _hd_doi       = Just i
                           , _hd_url       = Nothing
                           , _hd_uniqId    = Nothing
                           , _hd_uniqIdBdd = Nothing
                           , _hd_page      = Nothing
                           , _hd_title     = t
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
                           , _hd_language_iso2 = Just $ (pack . show) la
                           }
