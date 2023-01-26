{-|
Module      : Gargantext.Database.Schema.NgramsPostag
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams connection to the Database.

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.Ngrams
  where

import Codec.Serialise (Serialise())
import Control.Lens (over)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Map.Strict (fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, pack, strip)
import Database.PostgreSQL.Simple.FromField (returnError, ResultError(..))
import Gargantext.Core (HasDBid(..))
import Gargantext.Core.Types (TODO(..), Typed(..))
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Types
import Gargantext.Prelude
import Servant (FromHttpApiData(..), Proxy(..), ToHttpApiData(..))
import Test.QuickCheck (elements)
import Text.Read (read)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HashMap
import qualified Database.PostgreSQL.Simple as PGS


type NgramsId  = Int
type Size      = Int

data NgramsPoly id terms n = NgramsDB { _ngrams_id    :: !id
                                      , _ngrams_terms :: !terms
                                      , _ngrams_n     :: !n
                                      } deriving (Show)

type NgramsWrite = NgramsPoly (Maybe (Column SqlInt4))
                                   (Column SqlText)
                                   (Column SqlInt4)

type NgramsRead  = NgramsPoly (Column SqlInt4)
                              (Column SqlText)
                              (Column SqlInt4)

type NgramsReadNull = NgramsPoly (Column (Nullable SqlInt4))
                                 (Column (Nullable SqlText))
                                 (Column (Nullable SqlInt4))

type NgramsDB = NgramsPoly Int Text Int

$(makeAdaptorAndInstance "pNgramsDb"    ''NgramsPoly)
makeLenses ''NgramsPoly


ngramsTable :: Table NgramsWrite NgramsRead
ngramsTable = Table "ngrams" (pNgramsDb NgramsDB { _ngrams_id    = optionalTableField "id"
                                                 , _ngrams_terms = requiredTableField "terms"
                                                 , _ngrams_n     = requiredTableField "n"
                                                 }
                              )

-- | Main Ngrams Types
-- | Typed Ngrams
-- Typed Ngrams localize the context of the ngrams
-- ngrams in source  field  of document  has Sources Type
-- ngrams in authors field  of document  has Authors Type
-- ngrams in text    fields of documents has Terms   Type (i.e. either title or abstract)
data NgramsType = Authors | Institutes | Sources | NgramsTerms
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

instance Serialise NgramsType
instance FromJSON NgramsType
  where
    parseJSON (String "Authors")     = pure Authors
    parseJSON (String "Institutes")  = pure Institutes
    parseJSON (String "Sources")     = pure Sources
    parseJSON (String "Terms")       = pure NgramsTerms
    parseJSON (String "NgramsTerms") = pure NgramsTerms
    parseJSON _                      = mzero

instance FromJSONKey NgramsType where
   fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance ToJSON NgramsType
  where
    toJSON Authors     = String "Authors"
    toJSON Institutes  = String "Institutes"
    toJSON Sources     = String "Sources"
    toJSON NgramsTerms = String "Terms"

instance ToJSONKey NgramsType where
   toJSONKey = toJSONKeyText (pack . show)
instance FromHttpApiData NgramsType where
  parseUrlPiece n = pure $ (read . cs) n
instance ToHttpApiData NgramsType where
  toUrlPiece = pack . show
instance ToParamSchema NgramsType where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
instance Arbitrary NgramsType where
  arbitrary = elements [ minBound .. maxBound ]

-- map NgramsType to its assigned id
instance FromField NgramsType where
  fromField fld mdata =
    case B.unpack `fmap` mdata of
      Nothing -> returnError UnexpectedNull fld ""
      Just dat -> do
        n <- fromField fld mdata
        if (n :: Int) > 0 then
          case fromNgramsTypeId (NgramsTypeId n) of
            Nothing -> returnError ConversionFailed fld dat
            Just nt -> pure nt
        else
          returnError ConversionFailed fld dat
instance ToField NgramsType where
  toField nt = toField $ ngramsTypeId nt


ngramsTypes :: [NgramsType]
ngramsTypes = [minBound..]

instance ToSchema NgramsType
{-  where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nre_")
--}

newtype NgramsTypeId = NgramsTypeId Int
  deriving (Eq, Show, Ord, Num)
instance ToField NgramsTypeId where
  toField (NgramsTypeId n) = toField n
instance FromField NgramsTypeId where
  fromField fld mdata = do
    n <- fromField fld mdata
    if (n :: Int) > 0 then return $ NgramsTypeId n
                      else mzero
instance DefaultFromField (Nullable SqlInt4) NgramsTypeId
  where
    defaultFromField = fromPGSFromField

pgNgramsType :: NgramsType -> Column SqlInt4
pgNgramsType = pgNgramsTypeId . ngramsTypeId

pgNgramsTypeId :: NgramsTypeId -> Column SqlInt4
pgNgramsTypeId (NgramsTypeId n) = sqlInt4 n

ngramsTypeId :: NgramsType -> NgramsTypeId
ngramsTypeId Authors     = 1
ngramsTypeId Institutes  = 2
ngramsTypeId Sources     = 3
ngramsTypeId NgramsTerms = 4

fromNgramsTypeId :: NgramsTypeId -> Maybe NgramsType
fromNgramsTypeId id = lookup id
                    $ fromList [ (ngramsTypeId nt,nt)
                               | nt <- [minBound .. maxBound] :: [NgramsType]
                               ]

unNgramsTypeId :: NgramsTypeId -> Int
unNgramsTypeId (NgramsTypeId i) = i

toNgramsTypeId :: Int -> NgramsTypeId
toNgramsTypeId i = NgramsTypeId i

instance HasDBid NgramsType where
  toDBid   = unNgramsTypeId . ngramsTypeId
  fromDBid = fromMaybe (panic "NgramsType id not indexed") . fromNgramsTypeId . toNgramsTypeId

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | TODO put it in Gargantext.Core.Text.Ngrams
data Ngrams = UnsafeNgrams { _ngramsTerms :: Text
                           , _ngramsSize  :: Int
                           }
  deriving (Generic, Show, Eq, Ord)

instance Hashable Ngrams

makeLenses ''Ngrams
instance PGS.ToRow Ngrams where
  toRow (UnsafeNgrams t s) = [toField t, toField s]

instance FromField Ngrams where
  fromField fld mdata = do
    x <- fromField fld mdata
    pure $ text2ngrams x

instance PGS.ToRow Text where
  toRow t = [toField t]

text2ngrams :: Text -> Ngrams
text2ngrams txt = UnsafeNgrams txt' $ length $ splitOn " " txt'
  where
    txt' = strip txt


------------------------------------------------------------------------
-------------------------------------------------------------------------
-- | TODO put it in Gargantext.Core.Text.Ngrams
-- Named entity are typed ngrams of Terms Ngrams
data NgramsT a =
  NgramsT { _ngramsType :: NgramsType
          , _ngramsT    :: a
          } deriving (Generic, Show, Eq, Ord)

makeLenses ''NgramsT

instance Functor NgramsT where
  fmap = over ngramsT

-----------------------------------------------------------------------
withMap :: HashMap Text NgramsId -> Text -> NgramsId
withMap m n = maybe (panic $ "[G.D.S.Ngrams.withMap] Should not happen" <> (cs $ show n))
                    identity (HashMap.lookup n m)

indexNgramsT :: HashMap Text NgramsId -> NgramsT Ngrams -> NgramsT (Indexed Int Ngrams)
indexNgramsT = fmap . indexNgramsWith . withMap

-- | TODO replace NgramsT whith Typed NgramsType Ngrams
indexTypedNgrams :: HashMap Text NgramsId
                 -> Typed NgramsType Ngrams
                 -> Typed NgramsType (Indexed Int Ngrams)
indexTypedNgrams = fmap . indexNgramsWith . withMap

indexNgrams :: HashMap Text NgramsId -> Ngrams -> Indexed Int Ngrams
indexNgrams = indexNgramsWith . withMap

indexNgramsWith :: (Text -> NgramsId) -> Ngrams -> Indexed Int Ngrams
indexNgramsWith f n = Indexed (f $ _ngramsTerms n) n
