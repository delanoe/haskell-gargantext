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

module Gargantext.Viz.Chart
  where


import Data.Text (Text)
import Data.List (unzip, sortOn)
import Data.Map (toList)
import GHC.Generics (Generic)
import Gargantext.Prelude
import Gargantext.Database.Schema.NodeNode (selectDocsDates)
import Gargantext.Database.Utils
import Gargantext.Database.Types.Node (CorpusId)
import Gargantext.Text.Metrics.Count (occurrencesWith)


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

