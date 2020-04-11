{-|
Module      : Gargantext.Database.Schema.Ngrams
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams connection to the Database.

-}

{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Admin.Schema.Ngrams where

import Control.Lens (makeLenses, over)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString.Internal (ByteString)
import Data.Map (Map, fromList, lookup)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text, splitOn, pack)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow   (toRow)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import GHC.Generics (Generic)
import Gargantext.Database.Utils (Cmd, runPGSQuery, runOpaQuery, formatPGSQuery)
import Gargantext.Core.Types (TODO(..))
import Gargantext.Prelude
import Opaleye hiding (FromField)
import Servant (FromHttpApiData, parseUrlPiece, Proxy(..))
import Text.Read (read)
import Data.Swagger (ToParamSchema, toParamSchema, ToSchema)
import Prelude (Enum, Bounded, minBound, maxBound, Functor)
import qualified Database.PostgreSQL.Simple as PGS


type NgramsId    = Int
type NgramsTerms = Text
type Size        = Int

data NgramsPoly id terms n = NgramsDb { _ngrams_id    :: id
                                      , _ngrams_terms :: terms
                                      , _ngrams_n     :: n
                                      } deriving (Show)

type NgramsWrite = NgramsPoly (Maybe (Column PGInt4))
                                   (Column PGText)
                                   (Column PGInt4)

type NgramsRead  = NgramsPoly (Column PGInt4)
                              (Column PGText)
                              (Column PGInt4)

type NgramsReadNull = NgramsPoly (Column (Nullable PGInt4))
                                 (Column (Nullable PGText))
                                 (Column (Nullable PGInt4))

type NgramsDb = NgramsPoly Int Text Int

$(makeAdaptorAndInstance "pNgramsDb"    ''NgramsPoly)
makeLenses ''NgramsPoly


ngramsTable :: Table NgramsWrite NgramsRead
ngramsTable = Table "ngrams" (pNgramsDb NgramsDb { _ngrams_id    = optional "id"
                                                 , _ngrams_terms = required "terms"
                                                 , _ngrams_n     = required "n"
                                                 }
                              )

queryNgramsTable :: Query NgramsRead
queryNgramsTable = queryTable ngramsTable

dbGetNgramsDb :: Cmd err [NgramsDb]
dbGetNgramsDb = runOpaQuery queryNgramsTable

-- | Main Ngrams Types
-- | Typed Ngrams
-- Typed Ngrams localize the context of the ngrams
-- ngrams in source field of document has Sources Type
-- ngrams in authors field of document has Authors Type
-- ngrams in text (title or abstract) of documents has Terms Type
data NgramsType = Authors | Institutes | Sources | NgramsTerms
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

ngramsTypes :: [NgramsType]
ngramsTypes = [minBound..]

instance ToSchema NgramsType
{-  where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nre_")
-}

instance FromJSON NgramsType
instance FromJSONKey NgramsType where
   fromJSONKey = FromJSONKeyTextParser (parseJSON . String)
instance ToJSON NgramsType
instance ToJSONKey NgramsType where
   toJSONKey = toJSONKeyText (pack . show)

newtype NgramsTypeId = NgramsTypeId Int
  deriving (Eq, Show, Ord, Num)

instance ToField NgramsTypeId where
  toField (NgramsTypeId n) = toField n

instance FromField NgramsTypeId where
  fromField fld mdata = do
    n <- fromField fld mdata
    if (n :: Int) > 0 then return $ NgramsTypeId n
                      else mzero

instance FromHttpApiData NgramsType where
  parseUrlPiece n = pure $ (read . cs) n

instance ToParamSchema NgramsType where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)




instance QueryRunnerColumnDefault (Nullable PGInt4) NgramsTypeId
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

pgNgramsType :: NgramsType -> Column PGInt4
pgNgramsType = pgNgramsTypeId . ngramsTypeId

pgNgramsTypeId :: NgramsTypeId -> Column PGInt4
pgNgramsTypeId (NgramsTypeId n) = pgInt4 n

ngramsTypeId :: NgramsType -> NgramsTypeId
ngramsTypeId Authors     = 1
ngramsTypeId Institutes  = 2
ngramsTypeId Sources     = 3
ngramsTypeId NgramsTerms = 4

fromNgramsTypeId :: NgramsTypeId -> Maybe NgramsType
fromNgramsTypeId id = lookup id $ fromList [(ngramsTypeId nt,nt) | nt <- [minBound .. maxBound] :: [NgramsType]]

------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
data Ngrams = Ngrams { _ngramsTerms :: Text
                     , _ngramsSize  :: Int
           } deriving (Generic, Show, Eq, Ord)

makeLenses ''Ngrams
instance PGS.ToRow Ngrams where
  toRow (Ngrams t s) = [toField t, toField s]

text2ngrams :: Text -> Ngrams
text2ngrams txt = Ngrams txt $ length $ splitOn " " txt

-------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
-- Named entity are typed ngrams of Terms Ngrams
data NgramsT a =
  NgramsT { _ngramsType :: NgramsType
          , _ngramsT    :: a
          } deriving (Generic, Show, Eq, Ord)

makeLenses ''NgramsT

instance Functor NgramsT where
  fmap = over ngramsT
-----------------------------------------------------------------------
data NgramsIndexed =
  NgramsIndexed
  { _ngrams   :: Ngrams
  , _ngramsId :: NgramsId
  } deriving (Show, Generic, Eq, Ord)

makeLenses ''NgramsIndexed
------------------------------------------------------------------------
data NgramIds =
  NgramIds
  { ngramId    :: Int
  , ngramTerms :: Text
  } deriving (Show, Generic, Eq, Ord)

instance PGS.FromRow NgramIds where
  fromRow = NgramIds <$> field <*> field

----------------------
withMap :: Map NgramsTerms NgramsId -> NgramsTerms -> NgramsId
withMap m n = maybe (panic "withMap: should not happen") identity (lookup n m)

indexNgramsT :: Map NgramsTerms NgramsId -> NgramsT Ngrams -> NgramsT NgramsIndexed
indexNgramsT = fmap . indexNgramsWith . withMap

indexNgrams :: Map NgramsTerms NgramsId -> Ngrams -> NgramsIndexed
indexNgrams = indexNgramsWith . withMap

-- NP: not sure we need it anymore
indexNgramsTWith :: (NgramsTerms -> NgramsId) -> NgramsT Ngrams -> NgramsT NgramsIndexed
indexNgramsTWith = fmap . indexNgramsWith

indexNgramsWith :: (NgramsTerms -> NgramsId) -> Ngrams -> NgramsIndexed
indexNgramsWith f n = NgramsIndexed n (f $ _ngramsTerms n)

-- TODO-ACCESS: access must not be checked here but when insertNgrams is called.
insertNgrams :: [Ngrams] -> Cmd err (Map NgramsTerms NgramsId)
insertNgrams ns = fromList <$> map (\(NgramIds i t) -> (t, i)) <$> (insertNgrams' ns)

-- TODO-ACCESS: access must not be checked here but when insertNgrams' is called.
insertNgrams' :: [Ngrams] -> Cmd err [NgramIds]
insertNgrams' ns = runPGSQuery queryInsertNgrams (PGS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

insertNgrams_Debug :: [(NgramsTerms, Size)] -> Cmd err ByteString
insertNgrams_Debug ns = formatPGSQuery queryInsertNgrams (PGS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

----------------------
queryInsertNgrams :: PGS.Query
queryInsertNgrams = [sql|
    WITH input_rows(terms,n) AS (?)
    , ins AS (
       INSERT INTO ngrams (terms,n)
       SELECT * FROM input_rows
       ON CONFLICT (terms) DO NOTHING -- unique index created here
       RETURNING id,terms
       )

    SELECT id, terms
    FROM   ins
    UNION  ALL
    SELECT c.id, terms
    FROM   input_rows
    JOIN   ngrams c USING (terms);     -- columns of unique index
           |]


