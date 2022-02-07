{-|
Module      : Gargantext.Core.Viz.Phylo.API
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Viz.Phylo.API
  where

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Set (Set)
import Data.Time.Calendar (fromGregorian, diffGregorianDurationClip, cdMonths, diffDays, showGregorian)
import Gargantext.API.Ngrams.Tools (getRepo')
import Gargantext.API.Node.Corpus.Export (getContextNgrams)
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Types (Context)
import Gargantext.Core.Types.Main (ListType(MapTerm))
import Gargantext.Core.Viz.Phylo (TimeUnit(..), Date, Document(..))
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (CorpusId, ContextId)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import Prelude as Prelude
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

corpusIdtoDocuments :: TimeUnit -> CorpusId -> GargNoServer [Document]
corpusIdtoDocuments timeUnit corpusId = do
  docs <- selectDocNodes corpusId

  lId  <- defaultList corpusId
  repo <- getRepo' [lId]

  ngs_terms    <- getContextNgrams corpusId lId MapTerm NgramsTerms repo
  ngs_sources  <- getContextNgrams corpusId lId MapTerm Sources repo

  pure $ catMaybes
       $ List.map (\doc
                    -> context2phyloDocument timeUnit doc (ngs_terms, ngs_sources)
                  ) docs


context2phyloDocument :: TimeUnit
                      -> Context HyperdataDocument
                      -> (Map ContextId (Set NgramsTerm), Map ContextId (Set NgramsTerm))
                      -> Maybe Document
context2phyloDocument timeUnit context (ngs_terms, ngs_sources) = do
  let contextId = _context_id context
  (date, date') <- context2date context timeUnit
  text          <- Map.lookup contextId ngs_terms
  sources       <- Map.lookup contextId ngs_sources
  pure $ Document date date'
                  (toText text)
                   Nothing
                  (toText sources)
    where
      toText x = Set.toList $ Set.map unNgramsTerm x


context2date :: Context HyperdataDocument -> TimeUnit -> Maybe (Date, Text)
context2date context timeUnit = do
  let hyperdata =  _context_hyperdata context
  year  <- _hd_publication_year  hyperdata
  month <- _hd_publication_month hyperdata
  day   <- _hd_publication_day   hyperdata
  pure (toPhyloDate year month day timeUnit, toPhyloDate' year month day)


---------------
-- | Dates | --
---------------
toMonths :: Integer -> Int -> Int -> Date
toMonths y m d = fromIntegral $ cdMonths
               $ diffGregorianDurationClip (fromGregorian y    m d)
                                           (fromGregorian 0000 0 0)


toDays :: Integer -> Int -> Int -> Date
toDays y m d = fromIntegral
             $ diffDays (fromGregorian y m d) (fromGregorian 0000 0 0)


toPhyloDate :: Int -> Int -> Int -> TimeUnit -> Date
toPhyloDate y m d tu = case tu of
  Year  _ _ _ -> y
  Month _ _ _ -> toMonths (Prelude.toInteger y) m d
  Week  _ _ _ -> div (toDays (Prelude.toInteger y) m d) 7
  Day   _ _ _ -> toDays (Prelude.toInteger y) m d


-- Function to use in Database export
toPhyloDate' :: Int -> Int -> Int -> Text
toPhyloDate' y m d = pack
                   $ showGregorian
                   $ fromGregorian (Prelude.toInteger y) m d
