{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-- TODO-ACCESS: CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
Node API

-------------------------------------------------------------------
-- TODO-ACCESS: access by admin only.
--              At first let's just have an isAdmin check.
--              Later: check userId CanDeleteNodes Nothing
-- TODO-EVENTS: DeletedNodes [NodeId]
--              {"tag": "DeletedNodes", "nodes": [Int*]}


-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Table
  where

import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Swagger
import Data.Text (Text())
import GHC.Generics (Generic)
import Gargantext.API.Ngrams (TabType(..))
import Gargantext.Core.Types (Offset, Limit, TableResult(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Query.Facet (FacetDoc , runViewDocuments, OrderBy(..), runViewAuthorsDoc)
import Gargantext.Database.Action.Learn (FavOrTrash(..), moreLike)
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude -- (Cmd, CmdM)
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

type TableApi = Summary " Table API"
              :> ReqBody '[JSON] TableQuery
              :> Post    '[JSON] FacetTableResult

data TableQuery = TableQuery
  { tq_offset  :: Int
  , tq_limit   :: Int
  , tq_orderBy :: OrderBy
  , tq_view    :: TabType
  , tq_query  :: Text
  } deriving (Generic)

type FacetTableResult = TableResult FacetDoc

$(deriveJSON (unPrefix "tq_") ''TableQuery)

instance ToSchema TableQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "tq_")

instance Arbitrary TableQuery where
  arbitrary = elements [TableQuery 0 10 DateAsc Docs "electrodes"]


tableApi :: NodeId -> TableQuery -> Cmd err FacetTableResult
tableApi cId (TableQuery o l order ft "") = getTable cId (Just ft) (Just o) (Just l) (Just order)
tableApi cId (TableQuery o l order ft q) = case ft of
      Docs  -> searchInCorpus' cId False [q] (Just o) (Just l) (Just order)
      Trash -> searchInCorpus' cId True [q] (Just o) (Just l) (Just order)
      x     -> panic $ "not implemented in tableApi " <> (cs $ show x)

searchInCorpus' :: CorpusId
                -> Bool
                -> [Text]
                -> Maybe Offset
                -> Maybe Limit
                -> Maybe OrderBy
                -> Cmd err FacetTableResult
searchInCorpus' cId t q o l order = do
  docs <- searchInCorpus cId t q o l order
  countAllDocs  <- searchCountInCorpus cId t q
  pure $ TableResult { tr_docs = docs, tr_count = countAllDocs }


getTable :: NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err FacetTableResult
getTable cId ft o l order = do
  docs <- getTable' cId ft o l order
  -- TODO: Rewrite to use runCountOpaQuery and avoid (length allDocs)
  allDocs  <- getTable' cId ft Nothing Nothing Nothing
  pure $ TableResult { tr_docs = docs, tr_count = length allDocs }

getTable' :: NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getTable' cId ft o l order =
  case ft of
    (Just Docs)      -> runViewDocuments cId False o l order
    (Just Trash)     -> runViewDocuments cId True  o l order
    (Just MoreFav)   -> moreLike cId o l order IsFav
    (Just MoreTrash) -> moreLike cId o l order IsTrash
    x     -> panic $ "not implemented in getTable: " <> (cs $ show x)


getPair :: ContactId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getPair cId ft o l order =
  case ft of
    (Just Docs)  -> runViewAuthorsDoc cId False o l order
    (Just Trash) -> runViewAuthorsDoc cId True  o l order
    _     -> panic $ "not implemented: get Pairing" <> (cs $ show ft)

