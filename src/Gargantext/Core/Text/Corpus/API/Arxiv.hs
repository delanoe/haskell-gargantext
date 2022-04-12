{-|
Module      : Gargantext.Core.Text.Corpus.API.Arxiv
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}

module Gargantext.Core.Text.Corpus.API.Arxiv
    where

import Conduit
import Data.Either (Either(..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Client (ClientError)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

import qualified Arxiv as Arxiv
import qualified Network.Api.Arxiv as Ax


type Query = Text
type Limit = Arxiv.Limit

-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Lang -> Query -> Maybe Limit -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get la q l = do
  (cnt, resC) <- Arxiv.apiSimpleC l [Text.unpack q]
  pure $ Right (Just $ fromIntegral cnt, resC .| mapC (toDoc la))

toDoc :: Lang -> Arxiv.Result -> HyperdataDocument
toDoc l (Arxiv.Result { abstract
                      , authors = aus
                      --, categories
                      , doi
                      , id
                      , journal
                      --, primaryCategory
                      , publication_date
                      , title
                      --, total
                      , url
                      , year }
          ) = HyperdataDocument { _hd_bdd = Just "Arxiv"
                                , _hd_doi = Just $ Text.pack doi
                                , _hd_url = Just $ Text.pack url
                                , _hd_uniqId = Just $ Text.pack id
                                , _hd_uniqIdBdd = Nothing
                                , _hd_page = Nothing
                                , _hd_title = Just $ Text.pack title
                                , _hd_authors = authors aus
                                , _hd_institutes = institutes aus
                                , _hd_source = Just $ Text.pack journal
                                , _hd_abstract = Just $ Text.pack abstract
                                , _hd_publication_date = Just $ Text.pack publication_date
                                , _hd_publication_year = fromIntegral <$> year
                                , _hd_publication_month = Nothing  -- TODO parse publication_date
                                , _hd_publication_day = Nothing
                                , _hd_publication_hour = Nothing
                                , _hd_publication_minute = Nothing
                                , _hd_publication_second = Nothing
                                , _hd_language_iso2 = Just $ (Text.pack . show) l }
      where
        authors :: [Ax.Author] -> Maybe Text
        authors [] = Nothing
        authors aus' = Just $ (Text.intercalate ", ")
                            $ map Text.pack
                            $ map Ax.auName aus'

        institutes :: [Ax.Author] -> Maybe Text
        institutes [] = Nothing
        institutes aus' = Just $ (Text.intercalate ", ")
                               $ (map (Text.replace ", " " - "))
                               $ map Text.pack
                               $ map Ax.auFil aus'
