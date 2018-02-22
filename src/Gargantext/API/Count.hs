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
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Gargantext.API.Count
      where

import Gargantext.Prelude

import Data.Text (Text, pack)
import Servant
import GHC.Generics (Generic)
import Data.Aeson hiding (Error)



type CountAPI = Post '[JSON] Count

data Scraper = Pubmed | Hal 
  deriving (Generic)
instance FromJSON Scraper
instance ToJSON   Scraper


data Query = Query { query_query :: Text
                   , query_name  :: Maybe [Scraper]
                   }
                    deriving (Generic)
instance FromJSON Query
instance ToJSON   Query

data Error = Error { error_message :: Text
                   , error_code    :: Int
             } deriving (Generic)
instance FromJSON Error
instance ToJSON   Error

data Count = Count { count_name   :: Scraper
                   , count_count  :: Maybe Int
                   , count_errors :: Maybe [Error]
                    } 
                    deriving (Generic)
instance FromJSON Count
instance ToJSON   Count


count :: Query -> Handler Count
count _ = pure (Count Pubmed (Just 10) (Just [Error (pack "error message") 202]))


