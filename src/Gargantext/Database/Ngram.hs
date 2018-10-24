{-|
Module      : Gargantext.Database.Ngram
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

module Gargantext.Database.Ngram where

import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.List (find)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics (Generic)
import Data.ByteString.Internal (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ
import Gargantext.Database.Node (mkCmd, Cmd(..))
-- import Opaleye
import Prelude

import qualified Database.PostgreSQL.Simple as DPS
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))


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

type Ngram = Text
type NgramId = Int
type SizeN = Int

data NgramIds = NgramIds { ngramId :: Int
                         , ngramTerms :: Text
     } deriving (Show, Generic)

instance DPS.FromRow NgramIds where
  fromRow = NgramIds <$> field <*> field

----------------------
insertNgrams :: [(Ngram, SizeN)] -> Cmd [DPS.Only Int]
insertNgrams ns = mkCmd $ \conn -> DPS.query conn queryInsertNgrams (DPS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]


insertNgrams_Debug :: [(Ngram, SizeN)] -> Cmd ByteString
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

