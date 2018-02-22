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
{-# LANGUAGE DeriveAnyClass  #-}

module Gargantext.API.Count
      where

import Gargantext.Prelude

import Prelude (Bounded, Enum, minBound, maxBound)
import Data.Eq (Eq())
import Data.Text (Text, pack)
import Servant
import GHC.Generics (Generic)
import Data.Aeson hiding (Error)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
import Data.List (repeat,permutations)
-----------------------------------------------------------------------
type CountAPI = Post '[JSON] [Count]

-----------------------------------------------------------------------
data Scraper = Pubmed | Hal | IsTex | Isidore
  deriving (Eq, Show, Generic, Enum, Bounded)

scrapers :: [Scraper]
scrapers = [minBound..maxBound]

instance FromJSON Scraper
instance ToJSON   Scraper

instance Arbitrary Scraper where
    arbitrary = elements scrapers

-----------------------------------------------------------------------
-----------------------------------------------------------------------

data QueryBool = QueryBool Text
        deriving (Eq, Show, Generic)

queries :: [QueryBool]
queries =  [QueryBool (pack "(X OR X') AND (Y OR Y') NOT (Z OR Z')")]

instance Arbitrary QueryBool where
    arbitrary = elements queries

instance FromJSON QueryBool
instance ToJSON   QueryBool



data Query = Query { query_query :: QueryBool
                   , query_name  :: Maybe [Scraper]
                   }
                   deriving (Eq, Show, Generic)
instance FromJSON Query
instance ToJSON   Query
instance Arbitrary Query where
    arbitrary = elements [ Query q (Just n) 
                         | q <- queries
                         , n <- take 10 $ permutations scrapers
                         ]

-----------------------------------------------------------------------
-----------------------------------------------------------------------
data ErrorMessage = ErrorMessage Text
        deriving (Eq, Show, Generic)

errorMessages :: [ErrorMessage]
errorMessages = map (\m -> ErrorMessage (pack m)) $ [ "Ill formed query             "
                                                    , "API connexion error          "
                                                    , "Internal Gargantext Error    "
                                                    , "Connexion to Gargantext Error"
                                                   -- , "Token has expired            "
                                                    ] <> take 100 ( repeat ("No Error"))

instance Arbitrary ErrorMessage where
    arbitrary = elements errorMessages

instance FromJSON ErrorMessage
instance ToJSON   ErrorMessage

-----------------------------------------------------------------------
data Error = Error { error_message :: ErrorMessage
                   , error_code    :: Int
                   }
                   deriving (Eq, Show, Generic)
instance FromJSON Error
instance ToJSON   Error

errorCodes :: [Int]
errorCodes = [200,300,400,500]

errors :: [Error]
errors =  [ Error m c | m <- errorMessages
                      , c <- errorCodes
                      ]

instance Arbitrary Error where
    arbitrary = elements errors

-----------------------------------------------------------------------
-----------------------------------------------------------------------
data Count = Count { count_name   :: Scraper
                   , count_count  :: Maybe Int
                   , count_errors :: Maybe [Error]
                   } 
                   deriving (Eq, Show, Generic)

instance FromJSON Count
instance ToJSON   Count

instance Arbitrary Count where
    arbitrary = elements [ Count n (Just c) (Just [e]) | n <- scrapers
                                                       , c <- [100..1000]
                                                       , e <- errors
                                                       ]


-----------------------------------------------------------------------
count :: Query -> Handler [Count]
count _ = undefined
