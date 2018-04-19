{-|
Module      : Gargantext.Database.TextSearch
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

--{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.TextSearch where

import Prelude (print)
import Gargantext (connectGargandb)
import Data.Aeson
import Database.PostgreSQL.Simple
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Gargantext.Prelude


type TextQuery= Text
type ParentId = Int
type Limit    = Int
type Offset   = Int

data Order = Asc | Desc

toQuery :: Order -> Query
toQuery Asc  = "ASC"
toQuery Desc = "DESC"

-- TODO
-- FIX fav
-- ADD ngrams count
-- TESTS
textSearchQuery :: Order -> Query
textSearchQuery o = "SELECT n.id, n.hyperdata -> 'publication_date'   \
\                   , n.hyperdata -> 'title'                          \
\                   , n.hyperdata -> 'source'                         \
\                   , COALESCE(nn.score,null)                         \
\                      FROM nodes n                                   \
\            LEFT JOIN nodes_nodes nn  ON nn.node2_id = n.id          \
\              WHERE                                                  \
\                n.title_abstract @@ to_tsquery(?)                    \
\                AND n.parent_id = ?   AND n.typename  = 4            \
\                ORDER BY n.hyperdata -> 'publication_date' "
      <> toQuery o <>  " offset ? limit ?;"


textSearch :: Connection 
           -> TextQuery -> ParentId
           -> Limit -> Offset -> Order
           -> IO [(Int,Value,Value,Value, Maybe Int)]
textSearch conn q p l o ord = query conn (textSearchQuery ord) (q,p,o,l)

textSearchTest :: TextQuery -> IO ()
textSearchTest q = connectGargandb "gargantext.ini"
  >>= \conn -> textSearch conn q 421968 10 0 Asc
  >>= mapM_ print
