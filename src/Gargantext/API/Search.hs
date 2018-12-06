{-|
Module      : Gargantext.API.Count
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Count API part of Gargantext.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Gargantext.API.Search
      where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Error, fieldLabelModifier)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
-- import Control.Applicative ((<*>))
import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.TextSearch

-----------------------------------------------------------------------
data SearchQuery = SearchQuery { sq_query :: [Text]
                               , sq_parent_id :: Int
                               } deriving (Generic)
$(deriveJSON (unPrefix "sq_") ''SearchQuery)
instance ToSchema SearchQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] 472764]

-----------------------------------------------------------------------
data Author = Author { _a_name :: Text
                     , _a_id   :: Int
         } deriving (Generic)

$(deriveJSON (unPrefix "_a_") ''Author)

instance ToSchema Author where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}

arbitraryAuthor :: Author
arbitraryAuthor = Author "Jezequel" 1011669
instance Arbitrary Author where
  arbitrary = elements [arbitraryAuthor]

-----------------------------------------------------------------------
data SearchResult = SearchResult { sr_id      :: Int
                                 , sr_title   :: Text
                                 , sr_authors :: [Author]
                                 } deriving (Generic)
$(deriveJSON (unPrefix "sr_") ''SearchResult)
instance Arbitrary SearchResult where
  arbitrary = elements [SearchResult 1 "Title" [arbitraryAuthor]]

instance ToSchema SearchResult where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}

-----------------------------------------------------------------------

data SearchResults = SearchResults { srs_results :: [SearchResult]}
  deriving (Generic)
$(deriveJSON (unPrefix "srs_") ''SearchResults)

instance Arbitrary SearchResults where
  arbitrary = SearchResults <$> arbitrary

instance ToSchema SearchResults where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}



-----------------------------------------------------------------------
type SearchAPI = Post '[JSON] SearchResults
-----------------------------------------------------------------------

search :: Connection -> SearchQuery -> Handler SearchResults
search c (SearchQuery q pId) =
  liftIO $ SearchResults <$> map (\(i, _, t, _, _, _) -> SearchResult i (cs $ encode t) [arbitraryAuthor])
                         <$> textSearch c (toTSQuery q) pId 5 0 Desc


