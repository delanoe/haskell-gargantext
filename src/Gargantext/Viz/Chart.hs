{-|
Module      : Gargantext.Viz.Chart
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Viz.Chart
  where


import Data.Text (Text)
import Data.List (unzip, sortOn)
import Data.Map (toList)
import GHC.Generics (Generic)
import Gargantext.Prelude
import Gargantext.Database.Config
import Gargantext.Database.Schema.NodeNode (selectDocsDates)
import Gargantext.Database.Utils
import Gargantext.Database.Types.Node (CorpusId)
import Gargantext.Database.Node.Select
import Gargantext.Text.Metrics.Count (occurrencesWith)
import Gargantext.Core.Types.Main

-- Pie Chart
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.List as List
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.NTree
import Gargantext.Database.Metrics.NgramsByNode
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node
import Gargantext.Core.Types
import Gargantext.Database.Flow

import Servant


data Chart = ChartHisto | ChartScatter | ChartPie
  deriving (Generic)

-- TODO use UTCTime
data Histo = Histo { histo_dates :: [Text]
                   , histo_count :: [Int]
                   }
  deriving (Generic)

histoData :: CorpusId -> Cmd err Histo
histoData cId = do
  dates <- selectDocsDates cId
  let (ls, css) = unzip
                $ sortOn fst
                $ toList
                $ occurrencesWith identity dates
  pure (Histo ls css)


pieData :: FlowCmdM env err m
        => CorpusId -> NgramsType -> ListType
        -> m Histo
pieData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt
  let
    dico = filterListWithRoot lt ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ Map.toList dico
    group dico' x = case Map.lookup x dico' of
        Nothing -> x
        Just x' -> maybe x identity x'

  (_total,mapTerms) <- countNodesByNgramsWith (group dico)
                    <$> getNodesByNgramsOnlyUser cId (ls' <> ls) nt terms
  let (dates, count) = unzip $ map (\(t,(d,_)) -> (t, d)) $ Map.toList mapTerms
  pure (Histo dates (map round count))




treeData :: FlowCmdM env err m
        => CorpusId -> NgramsType -> ListType
        -> m [MyTree]
treeData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt
  
  let
    dico = filterListWithRoot lt ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ Map.toList dico
  
  cs' <- getNodesByNgramsOnlyUser cId (ls' <> ls) nt terms
  
  m  <- getListNgrams ls nt
  pure $ toTree lt cs' m


treeData' :: FlowCmdM env ServerError m
        => CorpusId -> NgramsType -> ListType
        -> m [MyTree]
treeData' cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt
  
  let
    dico = filterListWithRoot lt ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ Map.toList dico
  
  cs' <- getNodesByNgramsOnlyUser cId (ls' <> ls) nt terms
  
  m  <- getListNgrams ls nt
  pure $ toTree lt cs' m


