{-|
Module      : Gargantext.Database.Metrics.Count
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Count Ngrams by Context

-}

{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}

module Gargantext.Database.Metrics.Count where

import Control.Lens (view)
import Data.Map.Strict (Map, fromListWith, elems)
import Data.Text (Text)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Database.Schema.Node (HasNodeError(..))
import Gargantext.Prelude
import Gargantext.Core.Types.Main (listTypeId, ListType(..))
import Gargantext.Text.Metrics.Count (Coocs, coocOn)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Database.Types.Node (ListId, CorpusId)
import Gargantext.Database.Types.Node (NodeId)
import Gargantext.Database.Schema.Ngrams (NgramsId, NgramsType(..), ngramsTypeId, Ngrams(..), NgramsIndexed(..), ngrams, ngramsTerms)

getCoocByDocDev :: HasNodeError err => CorpusId -> ListId -> Cmd err (Map ([Text], [Text]) Coocs)
getCoocByDocDev cId lId = coocOn (\n-> [ view ( ngrams . ngramsTerms) n]) <$> getNgramsByDoc cId lId

getCoocByDoc :: CorpusId -> ListId -> Cmd err (Map (NgramsIndexed, NgramsIndexed) Coocs)
getCoocByDoc cId lId = coocOn identity <$> getNgramsByDoc cId lId


getNgramsByDoc :: CorpusId -> ListId -> Cmd err [[NgramsIndexed]]
getNgramsByDoc cId lId =
      elems
  <$> fromListWith (<>) 
  <$> map (\(nId, ngId, nt, n) -> (nId, [NgramsIndexed (Ngrams nt n) ngId]))
  <$> getNgramsByDocDb cId lId


getNgramsByDocDb :: CorpusId -> ListId -> Cmd err [(NodeId, NgramsId, Text, Int)]
getNgramsByDocDb cId lId = runPGSQuery query params
  where
    params = (cId, lId, listTypeId GraphList, ngramsTypeId NgramsTerms)
    query  = [sql|

    -- TODO add CTE
    SELECT n.id, ng.id, ng.terms, ng.n -- , list.parent_id
    FROM nodes n
    JOIN nodes_nodes  nn   ON nn.node2_id    = n.id
    JOIN nodes_ngrams nng  ON nng.node_id    = nn.node2_id
    JOIN nodes_ngrams list ON list.ngrams_id = nng.ngrams_id
    JOIN ngrams       ng   ON ng.id          = nng.ngrams_id
    WHERE nn.node1_id      = ? -- CorpusId
    AND   list.node_id     = ? -- ListId
    AND   list.list_type   = ? -- GraphListId
    AND   list.ngrams_type = ? -- NgramsTypeId

  |]
