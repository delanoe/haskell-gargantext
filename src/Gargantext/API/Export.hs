{-|
Module      : Gargantext.API.Export
Description : Get Metrics from Storage (Database like)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main exports of Gargantext:
- corpus
- document and ngrams
- lists

-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Export
  where

import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import Data.Set (Set)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Tools (filterListWithRoot, mapTermListRoot, getRepo)
import Gargantext.API.Types (GargNoServer)
import Gargantext.Core.Types --
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Config (userMaster)
import Gargantext.Database.Metrics.NgramsByNode (getNgramsByNodeOnlyUser)
import Gargantext.Database.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node (defaultList, HasNodeError)
import Gargantext.Database.Schema.NodeNode (selectDocNodes)
import Gargantext.Database.Types.Node (Node, HyperdataDocument(..), NodeId, ListId, CorpusId)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Corpus Export
data Corpus =
  Corpus { _c_corpus :: [Document]
         -- , _c_listVersion :: Int
         , _c_hash        :: Text
         } deriving (Generic)

-- | Document Export
data Document =
  Document { _d_document :: Node HyperdataDocument
           , _d_ngrams   :: [Text]
           -- , _d_hash     :: Text
           } deriving (Generic)

instance ToSchema Corpus where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_c_")

instance ToSchema Document where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_d_")

instance ToParamSchema Corpus where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Document where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

--------------------------------------------------
type API = Summary "Corpus Export"
            :> "export"
            :> QueryParam "listId"     ListId
            :> QueryParam "ngramsType" NgramsType
            :> Get '[JSON] Corpus

--------------------------------------------------
getCorpus :: CorpusId
          -> Maybe ListId
          -> Maybe NgramsType
          -> GargNoServer Corpus
getCorpus cId lId nt' = do

  let
    nt = case nt' of
      Nothing -> NgramsTerms
      Just  t -> t

  ns   <- Map.fromList
       <$> map (\n -> (_node_id n, n))
       <$> selectDocNodes cId
  repo <- getRepo
  ngs  <- getNodeNgrams cId lId nt repo
  let r = Map.intersectionWith (\a b -> Document a (Set.toList b)) ns ngs
  pure $ Corpus (Map.elems r) "HASH_TODO"

-- getCorpusNgrams :: CorpusId -> ListId -> 
-- Exports List
-- Version number of the list

getNodeNgrams :: HasNodeError err
        => CorpusId
        -> Maybe ListId
        -> NgramsType
        -> NgramsRepo
        -> Cmd err (Map NodeId (Set Text))
getNodeNgrams cId lId' nt repo = do
  lId <- case lId' of
    Nothing -> defaultList cId
    Just  l -> pure l

  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot GraphTerm $ mapTermListRoot [lId] nt repo
  r <- getNgramsByNodeOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)
  pure r


$(deriveJSON (unPrefix "_c_") ''Corpus)
$(deriveJSON (unPrefix "_d_") ''Document)

