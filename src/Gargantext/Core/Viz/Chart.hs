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
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Config
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.NodeContext (selectDocsDates)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Gargantext.Core.Text.Metrics.Count (occurrencesWith)

-- Pie Chart
import Gargantext.API.Ngrams.NgramsTree
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Metrics.NgramsByContext
import Gargantext.Database.Schema.Ngrams
import Gargantext.Core.Viz.Types
import qualified Data.HashMap.Strict as HashMap


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
  ts <- mapTermListRoot ls nt <$> getRepo' ls
  let
    dico = filterListWithRoot [lt] ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ HashMap.toList dico
    group dico' x = case HashMap.lookup x dico' of
        Nothing -> x
        Just x' -> maybe x identity x'

  (_total,mapTerms) <- countContextsByNgramsWith (group dico)
                    <$> getContextsByNgramsOnlyUser cId (ls' <> ls) nt terms
  let (dates, count) = V.unzip $
                       V.fromList $
                       List.sortOn snd $
                       (\(NgramsTerm t,(d,_)) -> (t, d)) <$>
                       HashMap.toList mapTerms
  pure (Histo dates (round <$> count))


treeData :: FlowCmdM env err m
        => CorpusId -> NgramsType -> ListType
        -> m (V.Vector NgramsTree)
treeData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt <$> getRepo' ls

  let
    dico = filterListWithRoot [lt] ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ HashMap.toList dico

  cs' <- getContextsByNgramsOnlyUser cId (ls' <> ls) nt terms

  m  <- getListNgrams ls nt
  pure $ V.fromList $ toTree lt cs' m

