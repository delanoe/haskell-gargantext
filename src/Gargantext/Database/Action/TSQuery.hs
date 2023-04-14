module Gargantext.Database.Action.TSQuery where

import Data.Aeson
import Data.List (intersperse)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text, words)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToField
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (Limit, Offset)
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Prelude
import Gargantext.Core.Text.Terms.Mono.Stem.En (stemIt)


newtype TSQuery = UnsafeTSQuery [Text]

-- | TODO [""] -> panic "error"
toTSQuery :: [Text] -> TSQuery
toTSQuery txt = UnsafeTSQuery $ map stemIt txt


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
\                n.search @@ (?::tsquery)                           \
\                AND (n.parent_id = ? OR nn.node1_id = ?)           \
\                AND n.typename  = ?                                \
\                ORDER BY n.hyperdata -> 'publication_date' ?       \
\            offset ? limit ?;"

-- | Text Search Function for Master Corpus
-- TODO : text search for user corpus
-- Example:
-- textSearchTest :: ParentId -> TSQuery -> Cmd err [(Int, Value, Value, Value, Value, Maybe Int)]
-- textSearchTest pId q = textSearch q pId 5 0 Asc
textSearch :: HasDBid NodeType
           => TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> Cmd err [(Int,Value,Value,Value, Value, Maybe Int)]
textSearch q p l o ord = runPGSQuery textSearchQuery (q,p,p,typeId,ord,o,l)
  where
    typeId = toDBid NodeDocument
