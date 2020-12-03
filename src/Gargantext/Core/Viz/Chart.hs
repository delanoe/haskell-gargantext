{-|
Module      : Gargantext.Core.Viz.Chart
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Viz.Chart
  where

import Data.List (sortOn)
import Data.Map (toList)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Config
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.NodeNode (selectDocsDates)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Gargantext.Core.Text.Metrics.Count (occurrencesWith)

-- Pie Chart
import Gargantext.API.Ngrams.NgramsTree
import Gargantext.API.Ngrams.Tools
import Gargantext.Core.Types
import Gargantext.Database.Action.Flow
import Gargantext.Database.Action.Metrics.NgramsByNode
import Gargantext.Database.Schema.Ngrams
import Gargantext.Core.Viz.Types

histoData :: CorpusId -> Cmd err Histo
histoData cId = do
  dates <- selectDocsDates cId
  let (ls, css) = V.unzip
                $ V.fromList
                $ sortOn fst -- TODO Vector.sortOn
                $ toList
                $ occurrencesWith identity dates
  pure (Histo ls css)


chartData :: FlowCmdM env err m
        => CorpusId -> NgramsType -> ListType
        -> m Histo
chartData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt <$> getRepo
  let
    dico = filterListWithRoot lt ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ Map.toList dico
    group dico' x = case Map.lookup x dico' of
        Nothing -> x
        Just x' -> maybe x identity x'

  (_total,mapTerms) <- countNodesByNgramsWith (group dico)
                    <$> getNodesByNgramsOnlyUser cId (ls' <> ls) nt terms
  let (dates, count) = V.unzip $ fmap (\(t,(d,_)) -> (t, d)) $ V.fromList $ Map.toList mapTerms
  pure (Histo (dates) (round <$> count))


treeData :: FlowCmdM env err m
        => CorpusId -> NgramsType -> ListType
        -> m [NgramsTree]
treeData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt <$> getRepo

  let
    dico = filterListWithRoot lt ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ Map.toList dico

  cs' <- getNodesByNgramsOnlyUser cId (ls' <> ls) nt terms

  m  <- getListNgrams ls nt
  pure $ toTree lt cs' m

