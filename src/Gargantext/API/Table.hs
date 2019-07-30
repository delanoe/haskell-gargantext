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

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
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
import Gargantext.Core.Types (Offset, Limit)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Facet (FacetDoc , runViewDocuments, OrderBy(..),runViewAuthorsDoc)
import Gargantext.Database.Learn (FavOrTrash(..), moreLike)
import Gargantext.Database.TextSearch
import Gargantext.Database.Types.Node
import Gargantext.Database.Utils -- (Cmd, CmdM)
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
type TableApi = Summary " Table API"
              :> ReqBody '[JSON] TableQuery
              :> Post '[JSON] [FacetDoc]

--{-
data TableQuery = TableQuery
  { tq_offset  :: Int
  , tq_limit   :: Int
  , tq_orderBy :: OrderBy
  , tq_view    :: TabType
  , tq_query  :: Text
  } deriving (Generic)

$(deriveJSON (unPrefix "tq_") ''TableQuery)

instance ToSchema TableQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = drop 3}

instance Arbitrary TableQuery where
  arbitrary = elements [TableQuery 0 10 DateAsc Docs "electrodes"]


tableApi :: NodeId -> TableQuery -> Cmd err [FacetDoc]
tableApi cId (TableQuery o l order ft "") = getTable cId (Just ft) (Just o) (Just l) (Just order)
tableApi cId (TableQuery o l order ft q) = case ft of
      Docs  -> searchInCorpus cId [q] (Just o) (Just l) (Just order)
      Trash -> panic "TODO search in Trash" -- TODO searchInCorpus cId q o l order
      x     -> panic $ "not implemented in tableApi " <> (cs $ show x)

getTable :: NodeId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getTable cId ft o l order =
  case ft of
    (Just Docs)  -> runViewDocuments cId False o l order
    (Just Trash) -> runViewDocuments cId True  o l order
    (Just MoreFav)   -> moreLike cId o l order IsFav
    (Just MoreTrash) -> moreLike cId o l order IsTrash
    x     -> panic $ "not implemented in getTable: " <> (cs $ show x)

getPairing :: ContactId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> Cmd err [FacetDoc]
getPairing cId ft o l order =
  case ft of
    (Just Docs)  -> runViewAuthorsDoc cId False o l order
    (Just Trash) -> runViewAuthorsDoc cId True  o l order
    _     -> panic $ "not implemented: get Pairing" <> (cs $ show ft)


