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
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..), ToHyperdataDocument, toHyperdataDocument)
import Gargantext.Prelude
import Gargantext.Database.GargDB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.JsonStream.Parser as P
import qualified Data.Text as Text

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
    toHyperdataDocument (GrandDebatReference { id, title, publishedAt, authorType, authorZipCode, responses }) =
      HyperdataDocument { _hd_bdd = Just "GrandDebat"
                        , _hd_doi = id
                        , _hd_url = Nothing
                        , _hd_uniqId = Nothing
                        , _hd_uniqIdBdd = Nothing
                        , _hd_page = Nothing
                        , _hd_title = title
                        , _hd_authors = authorType
                        , _hd_institutes = authorType
                        , _hd_source = authorZipCode
                        , _hd_abstract = toAbstract <$> responses
                        , _hd_publication_date = publishedAt
                        , _hd_publication_year = Nothing
                        , _hd_publication_month = Nothing
                        , _hd_publication_day = Nothing
                        , _hd_publication_hour = Nothing
                        , _hd_publication_minute = Nothing
                        , _hd_publication_second = Nothing
                        , _hd_language_iso2 = Just $ Text.pack $ show FR }
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



