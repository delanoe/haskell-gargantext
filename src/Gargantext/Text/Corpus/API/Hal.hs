{-|
Module      : Gargantext.Text.Corpus.API.Hal
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Corpus.API.Hal
    where

import Data.Maybe
import Data.Text (Text, pack, intercalate)

import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import qualified Gargantext.Text.Corpus.Parsers.Date as Date
import qualified HAL            as HAL
import qualified HAL.Client     as HAL
import qualified HAL.Doc.Corpus as HAL

get :: Lang -> Text -> Maybe Integer -> IO [HyperdataDocument]
get la q ml = do
  docs <- HAL.getMetadataWith q (fromIntegral <$> ml)
  either (panic . pack . show) (\d -> mapM (toDoc' la) $ HAL._docs d) docs

toDoc' :: Lang -> HAL.Corpus -> IO HyperdataDocument
toDoc' la (HAL.Corpus i t ab d s aus affs) = do
  (utctime, (pub_year, pub_month, pub_day)) <- Date.dateSplit la (maybe (Just "2019") Just d)
  pure $ HyperdataDocument (Just "Hal")
                   (Just $ pack $ show i)
                   Nothing
                   Nothing
                   Nothing
                   Nothing
                   (Just $ intercalate " " t)
                   (Just $ foldl (\x y -> x <> ", " <> y) "" aus)
                   (Just $ foldl (\x y -> x <> ", " <> y) "" affs)
                   (Just $ maybe "Nothing" identity s)
                   (Just $ intercalate " " ab)
                   (fmap (pack . show) utctime)
                   pub_year
                   pub_month
                   pub_day
                   Nothing
                   Nothing
                   Nothing
                  (Just $ (pack . show) la)

