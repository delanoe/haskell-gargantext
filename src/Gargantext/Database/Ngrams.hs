{-|
Module      : Gargantext.Database.Ngrams
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams connection to the Database.

-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Ngrams where

-- import Opaleye
import Control.Lens (makeLenses)
import Data.ByteString.Internal (ByteString)
import Data.Map (Map, fromList, lookup)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow   (toRow)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import GHC.Generics (Generic)
import Gargantext.Database.Node (mkCmd, Cmd(..))
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS


--data NgramPoly id terms n = NgramDb { ngram_id    :: id
--                                    , ngram_terms :: terms
--                                    , ngram_n     :: n
--                                    } deriving (Show)
--
--type NgramWrite = NgramPoly (Maybe (Column PGInt4))
--                                   (Column PGText)
--                                   (Column PGInt4)
--
--type NgramRead  = NgramPoly        (Column PGInt4)
--                                   (Column PGText)
--                                   (Column PGInt4)
--
----type Ngram = NgramPoly Int Text Int
--
-- $(makeAdaptorAndInstance "pNgram"    ''NgramPoly)
-- $(makeLensesWith abbreviatedFields   ''NgramPoly)
--
--ngramTable :: Table NgramWrite NgramRead
--ngramTable = Table "ngrams" (pNgram NgramDb { ngram_id    = optional "id"
--                                            , ngram_terms = required "terms"
--                                            , ngram_n     = required "n"
--                                            }
--                                )
--
--queryNgramTable :: Query NgramRead
--queryNgramTable = queryTable ngramTable
--
--dbGetNgrams :: PGS.Connection -> IO [NgramDb]
--dbGetNgrams conn = runQuery conn queryNgramTable

-- | Main Ngrams Types
-- | Typed Ngrams
-- Typed Ngrams localize the context of the ngrams
-- ngrams in source field of document has Sources Type
-- ngrams in authors field of document has Authors Type
-- ngrams in text (title or abstract) of documents has Terms Type
data NgramsType = Authors | Institutes | Sources | Terms
  deriving (Eq, Show)

ngramsTypeId :: NgramsType -> Int
ngramsTypeId Authors    = 1
ngramsTypeId Institutes = 2
ngramsTypeId Sources    = 3
ngramsTypeId Terms      = 4

type NgramsTerms = Text
type NgramsId    = Int
type Size       = Int

------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
data Ngrams = Ngrams { _ngramsTerms :: Text
                     , _ngramsSize  :: Int
           } deriving (Generic, Show)
instance Eq Ngrams where
  (==) = (==)
instance Ord Ngrams where
  compare = compare
makeLenses ''Ngrams
instance DPS.ToRow Ngrams where
  toRow (Ngrams t s) = [toField t, toField s]

text2ngrams :: Text -> Ngrams
text2ngrams txt = Ngrams txt $ length $ splitOn " " txt

-------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
-- Named entity are typed ngrams of Terms Ngrams
data NgramsT a =
  NgramsT { _ngramsType :: NgramsType
          , _ngramsT    :: a
          } deriving (Generic, Show)

instance Eq  (NgramsT a)
  where (==) = (==)
--    where NgramsT
--      t1 == t2
--      n1 == n2

instance Ord (NgramsT a) where compare = compare
makeLenses ''NgramsT


-----------------------------------------------------------------------
data NgramsIndexed =
  NgramsIndexed
  { _ngrams   :: Ngrams
  , _ngramsId :: NgramsId
  } deriving (Show, Generic)

instance Eq NgramsIndexed where
  (==) = (==)
instance Ord NgramsIndexed where
  compare = compare
makeLenses ''NgramsIndexed

------------------------------------------------------------------------
data NgramIds =
  NgramIds
  { ngramId    :: Int
  , ngramTerms :: Text
  } deriving (Show, Generic)

instance DPS.FromRow NgramIds where
  fromRow = NgramIds <$> field <*> field

----------------------
indexNgramsT :: Map NgramsTerms NgramsId -> NgramsT Ngrams -> NgramsT NgramsIndexed
indexNgramsT m ngrId = indexNgramsTWith f ngrId
  where
    f n = maybe (panic "indexNgramsT: should not happen") identity (lookup n m)

indexNgramsTWith :: (NgramsTerms -> NgramsId) -> NgramsT Ngrams-> NgramsT NgramsIndexed
indexNgramsTWith f (NgramsT t n) = NgramsT t (NgramsIndexed n ((f . _ngramsTerms) n))

insertNgrams :: [Ngrams] -> Cmd (Map NgramsTerms NgramsId)
insertNgrams ns = fromList <$> map (\(NgramIds i t) -> (t, i)) <$> (insertNgrams' ns)

insertNgrams' :: [Ngrams] -> Cmd [NgramIds]
insertNgrams' ns = mkCmd $ \conn -> DPS.query conn queryInsertNgrams (DPS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]


insertNgrams_Debug :: [(NgramsTerms, Size)] -> Cmd ByteString
insertNgrams_Debug ns = mkCmd $ \conn -> DPS.formatQuery conn queryInsertNgrams (DPS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

----------------------
queryInsertNgrams :: DPS.Query
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
