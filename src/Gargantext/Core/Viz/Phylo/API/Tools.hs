{-|
Module      : Gargantext.Core.Viz.Phylo.API
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Viz.Phylo.API.Tools
  where

import Data.Proxy
import Data.Aeson (Value, decodeFileStrict, eitherDecode, encode)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time.Calendar (fromGregorian, diffGregorianDurationClip, cdMonths, diffDays, showGregorian)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Gargantext.API.Ngrams.Prelude (getTermList)
import Gargantext.API.Ngrams.Tools (getRepo')
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.API.Node.Corpus.Export (getContextNgrams)
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Types (Context)
import Gargantext.Core.Types.Main (ListType(MapTerm))
import Gargantext.Core.Viz.Phylo (TimeUnit(..), Date, Document(..), PhyloConfig(..), Phylo)
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloStep)
import Gargantext.Core.Viz.Phylo.PhyloTools  ({-printIOMsg, printIOComment,-} setConfig)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataPhylo(..))
import Gargantext.Database.Admin.Types.Node (CorpusId, ContextId, PhyloId)
import Gargantext.Database.Query.Table.Node (defaultList, getNodeWith)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import Prelude             as Prelude
import System.Process      as Shell
import qualified Data.ByteString.Lazy                    as Lazy
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set


--------------------------------------------------------------------
getPhyloData :: PhyloId -> GargNoServer (Maybe Phylo)
getPhyloData phyloId = do
  nodePhylo <- getNodeWith phyloId (Proxy :: Proxy HyperdataPhylo)
  pure $ _hp_data $ _node_hyperdata nodePhylo

putPhylo :: PhyloId -> GargNoServer Phylo
putPhylo = undefined

savePhylo :: PhyloId -> GargNoServer ()
savePhylo = undefined

--------------------------------------------------------------------
phylo2dot2json :: Phylo -> IO Value
phylo2dot2json phylo = do

  let
    file_from    = "/tmp/fromPhylo.json"
    file_dot     = "/tmp/tmp.dot"
    file_to_json = "/tmp/toPhylo.json"

  _ <- dotToFile file_from (toPhyloExport phylo)
  _ <- Shell.callProcess "dot" ["-Tdot", "-o", file_dot, file_from]
  _ <- Shell.callProcess "dot" ["-Txdot_json", "-o", file_to_json, file_dot]

  maybeValue <- decodeFileStrict file_to_json
  print maybeValue
  _ <- Shell.callProcess "/bin/rm" ["-rf", file_from, file_to_json, file_dot]

  case maybeValue of
    Nothing -> panic "[G.C.V.Phylo.API.phylo2dot2json] Error no file"
    Just v  -> pure v



flowPhyloAPI :: PhyloConfig -> CorpusId -> GargNoServer Phylo
flowPhyloAPI config cId = do
  (mapList, corpus) <- corpusIdtoDocuments (timeUnit config) cId
  phyloWithCliques <- pure $ toPhyloStep corpus mapList config
  -- writePhylo phyloWithCliquesFile phyloWithCliques
  pure $ toPhylo (setConfig config phyloWithCliques)

--------------------------------------------------------------------
corpusIdtoDocuments :: TimeUnit -> CorpusId -> GargNoServer (TermList, [Document])
corpusIdtoDocuments timeUnit corpusId = do
  docs <- selectDocNodes corpusId
  lId  <- defaultList corpusId
  repo <- getRepo' [lId]

  ngs_terms    <- getContextNgrams corpusId lId MapTerm NgramsTerms repo
  ngs_sources  <- getContextNgrams corpusId lId MapTerm Sources repo

  termList <- getTermList lId MapTerm NgramsTerms

  let docs'= catMaybes
           $ List.map (\doc
                        -> context2phyloDocument timeUnit doc (ngs_terms, ngs_sources)
                      ) docs

  printDebug "corpusIdtoDocuments" (Prelude.map date docs')

  case termList of
    Nothing        -> panic "[G.C.V.Phylo.API] no termList found"
    Just termList' -> pure (termList', docs')

context2phyloDocument :: TimeUnit
                      -> Context HyperdataDocument
                      -> (Map ContextId (Set NgramsTerm), Map ContextId (Set NgramsTerm))
                      -> Maybe Document
context2phyloDocument timeUnit context (ngs_terms, ngs_sources) = do
  let contextId = _context_id context
  (date, date') <- context2date context timeUnit

  let
    toText x = Set.toList $ Set.map unNgramsTerm x

    text'    = maybe [] toText $ Map.lookup contextId ngs_terms
    sources' = maybe [] toText $ Map.lookup contextId ngs_sources

  pure $ Document date date' text' Nothing sources'


context2date :: Context HyperdataDocument -> TimeUnit -> Maybe (Date, Text)
context2date context timeUnit = do
  let hyperdata =  _context_hyperdata context
  year  <- _hd_publication_year  hyperdata
  month <- _hd_publication_month hyperdata
  day   <- _hd_publication_day   hyperdata
  pure (toPhyloDate year month day timeUnit, toPhyloDate' year month day timeUnit)


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
  _           -> panic "[G.C.V.Phylo.API] toPhyloDate"

toPhyloDate' :: Int -> Int -> Int -> TimeUnit -> Text
toPhyloDate' y m d tu = case tu of
  Epoch _ _ _ -> pack $ show $ posixSecondsToUTCTime $ fromIntegral y
  Year  _ _ _ -> pack $ showGregorian $ fromGregorian (toInteger y) m d
  Month _ _ _ -> pack $ showGregorian $ fromGregorian (toInteger y) m d
  Week  _ _ _ -> pack $ showGregorian $ fromGregorian (toInteger y) m d
  Day   _ _ _ -> pack $ showGregorian $ fromGregorian (toInteger y) m d

-- Utils

writePhylo :: [Char] -> Phylo -> IO ()
writePhylo path phylo = Lazy.writeFile path $ encode phylo


readPhylo :: [Char] -> IO Phylo
readPhylo path = do
  phyloJson <- (eitherDecode <$> readJson path) :: IO (Either String Phylo)
  case phyloJson of
    Left err -> do
      putStrLn err
      undefined
    Right phylo -> pure phylo


-- | To read and decode a Json file
readJson :: FilePath -> IO Lazy.ByteString
readJson path = Lazy.readFile path


