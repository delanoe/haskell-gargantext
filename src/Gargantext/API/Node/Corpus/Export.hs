{-|
Module      : Gargantext.API.Node.Corpus.Export
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

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Node.Corpus.Export
  where


import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Tools (filterListWithRoot, mapTermListRoot, getRepo)
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Crypto.Hash (hash)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Metrics.NgramsByNode (getNgramsByNodeOnlyUser)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (Node, NodeId, ListId, CorpusId)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.NodeNode (selectDocNodes)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node (_node_id, _node_hyperdata)
import Gargantext.Prelude
import Servant
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Corpus Export
data Corpus =
  Corpus { _c_corpus :: [Document]
         , _c_hash   :: Hash
         } deriving (Generic)

-- | Document Export
data Document =
  Document { _d_document :: Node HyperdataDocument
           , _d_ngrams   :: Ngrams
           , _d_hash     :: Hash
           } deriving (Generic)

data Ngrams =
  Ngrams { _ng_ngrams :: [Text]
         , _ng_hash   :: Hash
         } deriving (Generic)

type Hash = Text
-------
instance ToSchema Corpus where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_c_")

instance ToSchema Document where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_d_")

instance ToSchema Ngrams where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ng_")

-------
instance ToParamSchema Corpus where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Document where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Ngrams where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
--------------------------------------------------
type API = Summary "Corpus Export"
            :> "export"
            :> QueryParam "listId"     ListId
            :> QueryParam "ngramsType" NgramsType
            :> Get '[JSON] Corpus

--------------------------------------------------
-- | Hashes are ordered by Set
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
  let  -- uniqId is hash computed already for each document imported in database
    r = Map.intersectionWith (\a b -> Document a (Ngrams (Set.toList b) (hash b)) (d_hash a b)
                             ) ns ngs
          where
            d_hash  a b = hash [ fromMaybe "" (_hd_uniqId $ _node_hyperdata a)
                                       , hash b
                                       ]
  pure $ Corpus (Map.elems r) (hash $ List.map _d_hash
                                            $ Map.elems r
                              )

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
  let ngs = filterListWithRoot MapTerm $ mapTermListRoot [lId] nt repo
  r <- getNgramsByNodeOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)
  pure r


$(deriveJSON (unPrefix "_c_") ''Corpus)
$(deriveJSON (unPrefix "_d_") ''Document)
$(deriveJSON (unPrefix "_ng_") ''Ngrams)


-- TODO
-- Exports List
-- Version number of the list


