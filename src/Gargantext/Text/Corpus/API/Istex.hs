{-|
Module      : Gargantext.Text.Corpus.API.Istex
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Text.Corpus.API.Istex
    where

import Data.Either (either)
import Data.List (concat)
import Data.Maybe
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Node (HyperdataDocument(..))
import Gargantext.Prelude
import qualified Gargantext.Text.Corpus.Parsers.Date as Date
import qualified ISTEX        as ISTEX
import qualified ISTEX.Client as ISTEX

get :: Lang -> Text -> Maybe Integer -> IO [HyperdataDocument]
get la q ml = do
  docs <- ISTEX.getMetadataWith q (fromIntegral <$> ml)
  either (panic . pack . show) (toDoc' la) docs

toDoc' :: Lang -> ISTEX.Documents -> IO [HyperdataDocument]
toDoc' la docs' = do
  --printDebug "ISTEX" (ISTEX._documents_total docs')
  mapM (toDoc la) (ISTEX._documents_hits docs')

-- | TODO remove dateSplit here
-- TODO current year as default
toDoc :: Lang -> ISTEX.Document -> IO HyperdataDocument
toDoc la (ISTEX.Document i t a ab d s) = do
  (utctime, (pub_year, pub_month, pub_day)) <- Date.dateSplit la (maybe (Just "2019") (Just . pack . show) d)
  pure $ HyperdataDocument (Just "Istex")
                   (Just i)
                   Nothing
                   Nothing
                   Nothing
                   Nothing
                   t
                   (Just $ foldl (\x y -> x <> ", " <> y) "" (map ISTEX._author_name a))
                   (Just $ foldl (\x y -> x <> ", " <> y) "" (concat $ (map ISTEX._author_affiliations) a))
                   (Just $ foldl (\x y -> x <> ", " <> y) "" (catMaybes $ map ISTEX._source_title s))
                   ab
                   (fmap (pack . show) utctime)
                   pub_year
                   pub_month
                   pub_day
                   Nothing
                   Nothing
                   Nothing
                  (Just $ (pack . show) la)
