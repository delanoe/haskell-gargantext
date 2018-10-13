{-|
Module      : Gargantext.Database.TextSearch
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.TextSearch where

import Prelude (print)

import Control.Monad

import Data.Aeson
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text, words)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Gargantext.Database.Utils (connectGargandb)
import Gargantext.Prelude

newtype TSQuery = UnsafeTSQuery [Text]

-- | TODO [""] -> panic "error"
toTSQuery :: [Text] -> TSQuery
toTSQuery txt = UnsafeTSQuery txt

instance IsString TSQuery
  where
    fromString = UnsafeTSQuery . words . cs


instance ToField TSQuery
  where
    toField (UnsafeTSQuery xs)
      = Many  $ intersperse (Plain " && ")
              $ map (\q -> Many [ Plain "plainto_tsquery("
                                , Escape (cs q)
                                , Plain ")"
                                ]
                    ) xs

type ParentId = Int
type Limit    = Int
type Offset   = Int
data Order    = Asc | Desc

instance ToField Order
  where
    toField Asc  = Plain "ASC"
    toField Desc = Plain "DESC"

-- TODO
-- FIX fav
-- ADD ngrams count
-- TESTS
textSearchQuery :: Query
textSearchQuery = "SELECT n.id, n.hyperdata->'publication_year'     \
\                   , n.hyperdata->'title'                          \
\                   , n.hyperdata->'source'                         \
\                   , n.hyperdata->'authors'                        \
\                   , COALESCE(nn.score,null)                       \
\                      FROM nodes n                                 \
\            LEFT JOIN nodes_nodes nn  ON nn.node2_id = n.id        \
\              WHERE                                                \
\                n.search @@ (?::tsquery)                   \
\                AND n.parent_id = ?   AND n.typename  = 40         \
\                ORDER BY n.hyperdata -> 'publication_date' ?       \
\            offset ? limit ?;"

textSearch :: Connection 
           -> TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> IO [(Int,Value,Value,Value, Value, Maybe Int)]
textSearch conn q p l o ord = query conn textSearchQuery (q,p,ord, o,l)

textSearchTest :: ParentId -> TSQuery -> IO ()
textSearchTest pId q = connectGargandb "gargantext.ini"
  >>= \conn -> textSearch conn q pId 5 0 Asc
  >>= mapM_ print
