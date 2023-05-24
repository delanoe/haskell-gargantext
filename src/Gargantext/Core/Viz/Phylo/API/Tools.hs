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

import Data.Aeson (Value, decodeFileStrict, eitherDecode, encode)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time.Calendar (fromGregorian, diffGregorianDurationClip, cdMonths, diffDays, showGregorian)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Gargantext.API.Ngrams.Prelude (getTermList)
import Gargantext.API.Ngrams.Tools (getRepo)
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.API.Node.Corpus.Export (getContextNgrams)
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Types (Context)
-- import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main (ListType(MapTerm))
import Gargantext.Core.Viz.Phylo (TimeUnit(..), Date, Document(..), PhyloConfig(..), Phylo)
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools  ({-printIOMsg, printIOComment,-} setConfig)
-- import Gargantext.Database.Action.Flow (getOrMk_RootWithCorpus)
-- import Gargantext.Database.Admin.Config (userMaster)
-- import Gargantext.Database.Admin.Types.Hyperdata (HyperdataCorpus)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataPhylo(..))
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (CorpusId, ContextId, PhyloId)
import Gargantext.Database.Query.Table.Node (defaultList, getNodeWith)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Prelude
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Process      as Shell
import qualified Data.ByteString.Lazy                    as Lazy
import qualified Data.List as List
import qualified Data.Map.Strict  as Map
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
  withTempDirectory "/tmp" "phylo" $ \dirPath -> do
    let fileFrom = dirPath </> "phyloFrom.dot"
        fileDot  = dirPath </> "phylo.dot"
        fileToJson = dirPath </> "output.json"

    dotToFile fileFrom (toPhyloExport phylo)

    -- parsing a file can be done with:
    -- runParser' (Data.GraphViz.Parsing.parse :: Parse (Data.GraphViz.DotGraph Text)) $ TL.fromStrict f

    Shell.callProcess "dot" ["-Tdot", "-o", fileDot, fileFrom]
    Shell.callProcess "dot" ["-Txdot_json", "-o", fileToJson, fileDot]

    maybeValue <- decodeFileStrict fileToJson
    print maybeValue

    case maybeValue of
      Nothing -> panic "[G.C.V.Phylo.API.phylo2dot2json] Error no file"
      Just v  -> pure v


flowPhyloAPI :: PhyloConfig -> CorpusId -> GargNoServer Phylo
flowPhyloAPI config cId = do
  (_, corpus) <- corpusIdtoDocuments (timeUnit config) cId
  let phyloWithCliques = toPhyloWithoutLink corpus config
  -- writePhylo phyloWithCliquesFile phyloWithCliques
  pure $ toPhylo (setConfig config phyloWithCliques)

--------------------------------------------------------------------
corpusIdtoDocuments :: TimeUnit -> CorpusId -> GargNoServer (TermList, [Document])
corpusIdtoDocuments timeUnit corpusId = do
  docs <- selectDocNodes corpusId
  printDebug "docs *****" (length docs)
  lId  <- defaultList corpusId

{-
  (_masterUserId, _masterRootId, masterCorpusId) <- getOrMk_RootWithCorpus
                                           (UserName userMaster)
                                           (Left "")
                                           (Nothing :: Maybe HyperdataCorpus)
  mListId <- defaultList masterCorpusId
  repo <- getRepo [mListId,lId]
-}
  repo <- getRepo [lId]
  -- ngs_terms'   <- getContextNgrams corpusId mListId MapTerm NgramsTerms repo

  ngs_terms    <- getContextNgrams corpusId lId MapTerm NgramsTerms repo
  printDebug "Size ngs_coterms *****" (length ngs_terms)

  ngs_sources  <- getContextNgrams corpusId lId MapTerm Sources repo
  printDebug "Size ngs_sources Map Sources *****" (length ngs_sources)

  termList <- getTermList lId MapTerm NgramsTerms
  printDebug "Size ngs_terms List Map Ngrams *****" (length <$> termList)

  let docs'= catMaybes
           $ List.map (\doc
                        -> context2phyloDocument timeUnit doc (ngs_terms {-<> ngs_terms'-}, ngs_sources)
                      ) docs

  -- printDebug "corpusIdtoDocuments" (Prelude.map date docs')

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
  Year  {} -> y
  Month {} -> toMonths (Prelude.toInteger y) m d
  Week  {} -> div (toDays (Prelude.toInteger y) m d) 7
  Day   {} -> toDays (Prelude.toInteger y) m d
  _        -> panic "[G.C.V.Phylo.API] toPhyloDate"

toPhyloDate' :: Int -> Int -> Int -> TimeUnit -> Text
toPhyloDate' y _m _d (Epoch {}) = pack $ show $ posixSecondsToUTCTime $ fromIntegral y
toPhyloDate' y  m  d _          = pack $ showGregorian $ fromGregorian (toInteger y) m d

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
readJson = Lazy.readFile
