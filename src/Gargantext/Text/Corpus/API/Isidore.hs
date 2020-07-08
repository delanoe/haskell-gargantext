{-|
Module      : Gargantext.Text.Corpus.API.Isidore
Description : To query French Humanities publication database from its API
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Text.Corpus.API.Isidore where

import System.FilePath (FilePath())
import Data.Text (Text)
import qualified Data.Text as Text
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import Isidore.Client
import Servant.Client
import qualified Gargantext.Text.Corpus.Parsers.Date as Date
import qualified Isidore as Isidore
import Gargantext.Text.Corpus.Parsers.CSV (writeDocs2Csv)
import Gargantext.Text.Corpus.Parsers (cleanText)

-- | TODO work with the ServantErr
get :: Lang -> Maybe Isidore.Limit
    -> Maybe Isidore.TextQuery -> Maybe Isidore.AuthorQuery
    -> IO [HyperdataDocument]
get la l q a = do
  let
    printErr (DecodeFailure e _) = panic e
    printErr e                   = panic (cs $ show e)
    
    toIsidoreDocs :: Reply -> [IsidoreDoc]
    toIsidoreDocs (ReplyOnly r) = [r]
    toIsidoreDocs (Replies  rs) = rs

  iDocs <- either printErr _content <$> Isidore.get l q a
  
  hDocs <- mapM (\d -> isidoreToDoc la d) (toIsidoreDocs iDocs)
  pure hDocs

isidore2csvFile :: FilePath -> Lang -> Maybe Isidore.Limit
    -> Maybe Isidore.TextQuery -> Maybe Isidore.AuthorQuery
    -> IO ()
isidore2csvFile fp la li tq aq = do
  hdocs <- get la li tq aq
  writeDocs2Csv fp hdocs

isidoreToDoc :: Lang -> IsidoreDoc -> IO HyperdataDocument
isidoreToDoc l (IsidoreDoc t a d u s as) = do
  let
    author :: Author -> Text
    author (Author fn ln) = (_name fn) <> ", " <> (_name ln)
    author (Authors aus) = Text.intercalate ". " $ map author aus
    
    creator2text :: Creator -> Text
    creator2text (Creator au)   = author au
    creator2text (Creators aus') = Text.intercalate ". " $ map author aus'

    langText :: LangText -> Text
    langText (LangText _l t1) = t1
    langText (OnlyText t2   ) = t2
    langText (ArrayText ts  ) = Text.intercalate " " $ map langText ts
    
  (utcTime, (pub_year, pub_month, pub_day)) <- Date.dateSplit l (maybe (Just "2019") (Just) d)
    
  pure $ HyperdataDocument (Just "Isidore")
                 Nothing
                 u
                 Nothing
                 Nothing
                 Nothing
                 (Just $ cleanText $ langText t)
                 (creator2text <$> as)
                 Nothing
                 (Just $ maybe "Nothing" identity $ _sourceName <$> s)
                 (cleanText <$> langText    <$> a)
                 (fmap (Text.pack . show) utcTime)
                 (pub_year)
                 (pub_month)
                 (pub_day)
                 Nothing
                 Nothing
                 Nothing
                 (Just $ (Text.pack . show) l)


