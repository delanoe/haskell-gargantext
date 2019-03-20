{-|
Module      : Gargantext.Text.Parsers.GrandDebat
Description : Grand Debat Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO: create a separate Lib.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Gargantext.Text.Parsers.GrandDebat
  where

import GHC.IO (FilePath)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.JsonStream.Parser as P
--import Data.Either (either)
import Data.Maybe (Maybe())
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as DBL
import GHC.Generics (Generic)
import Gargantext.Prelude
import Gargantext.Database.Types.Node 
import Gargantext.Core (Lang(..))


data GrandDebatReference = GrandDebatReference
  { id        :: !(Maybe Text)
  , reference :: !(Maybe Text)
  , title     :: !(Maybe Text)

  , createdAt :: !(Maybe Text)
  , publishedAt   :: !(Maybe Text)
  , updatedAt     :: !(Maybe Text)
  
  , trashed       :: !(Maybe Bool)
  , trashedStatus :: !(Maybe Text)
  
  , authorId      :: !(Maybe Text)
  , authorType    :: !(Maybe Text)
  , authorZipCode :: !(Maybe Text)
  
  , responses     :: !(Maybe [GrandDebatResponse])
  }
  deriving (Show, Generic)


data GrandDebatResponse = GrandDebatResponse
  { questionId     :: !(Maybe Text)
  , questionTitle  :: !(Maybe Text)
  , value          :: !(Maybe Text)
  , formattedValue :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON GrandDebatResponse
instance FromJSON GrandDebatReference

instance ToJSON GrandDebatResponse
instance ToJSON GrandDebatReference


instance ToHyperdataDocument GrandDebatReference
  where
    toHyperdataDocument (GrandDebatReference id' _ref title'
                                   _createdAt' publishedAt' _updatedAt
                                   _trashed _trashedStatus
                                   _authorId authorType' authorZipCode'
                                   responses') =
      HyperdataDocument (Just "GrandDebat") id'
                         Nothing Nothing Nothing Nothing
                         title' authorType' authorType' authorZipCode'
                        (toAbstract <$> responses')
                         publishedAt'
                         Nothing Nothing Nothing Nothing Nothing Nothing
                        (Just $ Text.pack $ show FR)
        where
          toAbstract = (Text.intercalate " . ") . ((filter (/= "")) . (map toSentence))
          toSentence (GrandDebatResponse _id _qtitle _qvalue r) = case r of
            Nothing -> ""
            Just r' -> case Text.length r' > 10 of
                True  -> r'
                False -> ""

class ReadFile a
  where
    readFile :: FilePath -> IO a

instance ReadFile [GrandDebatReference]
  where
    --readFile fp = maybe [] identity <$> decode <$> DBL.readFile fp
    --readFile fp = either (panic . Text.pack) identity <$> P.eitherDecode <$> DBL.readFile fp
    readFile fp = P.parseLazyByteString (P.arrayOf P.value) <$> DBL.readFile fp
