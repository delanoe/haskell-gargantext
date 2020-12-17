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

module Gargantext.API.Node.Corpus.Export
  where


import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Node.Corpus.Export.Types
import Gargantext.API.Ngrams.Types
import Gargantext.API.Ngrams.Tools (filterListWithRoot, mapTermListRoot, getRepo)
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Prelude.Crypto.Hash (hash)
import Gargantext.Core.Types
import Gargantext.Database.Action.Metrics.NgramsByNode (getNgramsByNodeOnlyUser)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.NodeNode (selectDocNodes)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node (_node_id, _node_hyperdata)
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

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
                             ) ns (Map.map (Set.map unNgramsTerm) ngs)
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
        -> Cmd err (Map NodeId (Set NgramsTerm))
getNodeNgrams cId lId' nt repo = do
  lId <- case lId' of
    Nothing -> defaultList cId
    Just  l -> pure l

  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot MapTerm $ mapTermListRoot [lId] nt repo
  -- TODO HashMap
  r <- getNgramsByNodeOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)
  pure r

-- TODO
-- Exports List
-- Version number of the list
