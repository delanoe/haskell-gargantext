{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.GrandDebat
Description : Grand Debat Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

_flowCorpusDebat :: FlowCmdM env err m
                 => User -> Either CorpusName [CorpusId]
                 -> Limit -> FilePath
                 -> m CorpusId
_flowCorpusDebat u n l fp = do
  docs <- liftBase ( splitEvery 500
                 <$> take l
                 <$> readFile' fp
                 :: IO [[GD.GrandDebatReference ]]
                 )
  flowCorpus u n (Multi FR) (map (map toHyperdataDocument) docs)


-}


module Gargantext.Core.Text.Corpus.Parsers.GrandDebat
  where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as DBL
import qualified Data.JsonStream.Parser as P
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..), ToHyperdataDocument, toHyperdataDocument)
import Gargantext.Prelude
import Gargantext.Prelude.Utils


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

instance ReadFile [GrandDebatReference]
  where
    -- | read json: 3 version below are working but with increased optimization
    --readFile fp = maybe [] identity <$> decode <$> DBL.readFile fp
    --readFile fp = either (panic . Text.pack) identity <$> P.eitherDecode <$> DBL.readFile fp
    readFile' fp = P.parseLazyByteString (P.arrayOf P.value) <$> DBL.readFile fp



