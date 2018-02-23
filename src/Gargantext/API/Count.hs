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
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE DeriveAnyClass              #-}

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
type CountAPI = Post '[JSON] Counts

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

type Error  = Text
type Errors = [Error]

data Message = Message Integer Errors
        deriving (Eq, Show, Generic)

toMessage :: [(Integer, [Text])] -> [Message]
toMessage = map (\(c,es) -> Message c es)

messages :: [Message]
messages =  toMessage $ [ (400, ["Ill formed query             "])
                        , (300, ["API connexion error          "])
                        , (300, ["Internal Gargantext Error    "])
                        , (300, ["Connexion to Gargantext Error"])
                        , (300, ["Token has expired            "])
                        ] <> take 10 ( repeat (200, [""]))

instance Arbitrary Message where
    arbitrary = elements messages

instance FromJSON Message
instance ToJSON   Message

-----------------------------------------------------------------------
-----------------------------------------------------------------------
data Counts = Counts [Count]
                   deriving (Eq, Show, Generic)

instance FromJSON Counts
instance ToJSON   Counts

data Count = Count { count_name    :: Scraper
                   , count_count   :: Maybe Int
                   , count_message :: Maybe Message
                   }
                   deriving (Eq, Show, Generic)

instance FromJSON Count
instance ToJSON   Count

instance Arbitrary Counts where
    arbitrary = elements $ select
                         $ map Counts 
                         $ map (\xs -> zipWith (\s (c,m) -> Count s c m) scrapers xs) 
                         $ chunkAlong (length scrapers) 1 $  (map filter' countOrErrors)
        where
            select xs = (take 10 xs) <> (take 10 $ drop 100 xs)
            countOrErrors = [ (c,e) | c <- [500..1000], e <- reverse messages]
            filter' (c,e) = case e of
                              Message 200 _ -> (Just c , Nothing     )
                              message       -> (Nothing, Just message)

-----------------------------------------------------------------------
count :: Query -> Handler Counts
count _ = undefined
