{-|
Module      : Main.hs
Description : Gargantext starter binary with Adaptative Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Adaptative Phylo binaries
 -}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Strict             #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson
import Data.Either (Either(..))
import Data.List  (concat, nub, isSuffixOf)
import Data.String (String)
import GHC.IO (FilePath) 
import qualified Prelude as Prelude
import System.Environment
import System.Directory (listDirectory,doesFileExist)
import Data.Text  (Text, unwords, unpack, replace, pack)
import Data.Time.Calendar (fromGregorian, diffGregorianDurationClip, cdMonths, diffDays, showGregorian)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Vector
import qualified Data.Text as T

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year, csv_publication_month, csv_publication_day,
  csv'_source, csv'_title, csv'_abstract, csv'_publication_year, csv'_publication_month, csv'_publication_day, csv'_weight)
import qualified Gargantext.Core.Text.Corpus.Parsers.CSV as Csv
import Gargantext.Core.Text.Corpus.Parsers (FileFormat(..),parseFile)
import Gargantext.Core.Text.List.Formats.CSV (csvMapTermList)
import Gargantext.Core.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Core.Viz.AdaptativePhylo
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloStep)
import Gargantext.Core.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment, setConfig)
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
-- import Gargantext.API.Ngrams.Prelude (toTermList)

-- import Debug.Trace (trace)

data PhyloStage = PhyloWithCliques | PhyloWithLinks deriving (Show)

---------------
-- | Tools | --
---------------


-- | To get all the files in a directory or just a file
getFilesFromPath :: FilePath -> IO([FilePath])
getFilesFromPath path = do 
  if (isSuffixOf "/" path) 
    then (listDirectory path)
    else return [path]

---------------
-- | Dates | --
---------------    


toMonths :: Integer -> Int -> Int -> Date
toMonths y m d = fromIntegral $ cdMonths 
              $ diffGregorianDurationClip (fromGregorian y m d) (fromGregorian 0000 0 0)


toDays :: Integer -> Int -> Int -> Date
toDays y m d = fromIntegral 
             $ diffDays (fromGregorian y m d) (fromGregorian 0000 0 0)


toPhyloDate :: Int -> Int -> Int -> TimeUnit -> Date
toPhyloDate y m d tu = case tu of 
  Year  _ _ _ -> y
  Month _ _ _ -> toMonths (Prelude.toInteger y) m d
  Week  _ _ _ -> div (toDays (Prelude.toInteger y) m d) 7
  Day   _ _ _ -> toDays (Prelude.toInteger y) m d


toPhyloDate' :: Int -> Int -> Int -> Text
toPhyloDate' y m d = pack $ showGregorian $ fromGregorian (Prelude.toInteger y) m d


--------------
-- | Json | --
--------------


-- | To read and decode a Json file
readJson :: FilePath -> IO Lazy.ByteString
readJson path = Lazy.readFile path


----------------
-- | Parser | --
----------------

-- | To filter the Ngrams of a document based on the termList
termsInText :: Patterns -> Text -> [Text]
termsInText pats txt = nub $ concat $ map (map unwords) $ extractTermsWithList pats txt


-- | To transform a Wos file (or [file]) into a list of Docs
wosToDocs :: Int -> Patterns -> TimeUnit -> FilePath -> IO [Document]
wosToDocs limit patterns time path = do 
  files <- getFilesFromPath path
  let parseFile' file = do
        eParsed <- parseFile WOS (path <> file)
        case eParsed of
          Right ps -> pure ps
          Left e   -> panic $ "Error: " <> (pack e)
  take limit
    <$> map (\d -> let title = fromJust $ _hd_title d
                       abstr = if (isJust $ _hd_abstract d)
                               then fromJust $ _hd_abstract d
                               else ""
                    in Document (toPhyloDate
                                  (fromIntegral $ fromJust $ _hd_publication_year d) 
                                  (fromJust $ _hd_publication_month d) 
                                  (fromJust $ _hd_publication_day d) time)  
                                (toPhyloDate'
                                  (fromIntegral $ fromJust $ _hd_publication_year d) 
                                  (fromJust $ _hd_publication_month d) 
                                  (fromJust $ _hd_publication_day d)) 
                                (termsInText patterns $ title <> " " <> abstr) Nothing []) 
    <$> concat 
    <$> mapConcurrently (\file -> 
          filter (\d -> (isJust $ _hd_publication_year d)
                     && (isJust $ _hd_title d))
             <$> parseFile' file) files


-- To transform a Csv file into a list of Document
csvToDocs :: CorpusParser -> Patterns -> TimeUnit -> FilePath -> IO [Document]
csvToDocs parser patterns time path = 
  case parser of
    Wos  _     -> undefined
    Csv  limit -> do
      eR <- Csv.readFile path
      case eR of
        Right r ->
          pure $ Vector.toList
            $ Vector.take limit
            $ Vector.map (\row -> Document (toPhyloDate  (Csv.unIntOrDec $ csv_publication_year row) (csv_publication_month row) (csv_publication_day row) time)
                                           (toPhyloDate' (Csv.unIntOrDec $ csv_publication_year row) (csv_publication_month row) (csv_publication_day row))
                                           (termsInText patterns $ (csv_title row) <> " " <> (csv_abstract row))
                                           Nothing
                                           []
                         ) $ snd r
        Left e -> panic $ "Error: " <> (pack e)
    Csv' limit -> Vector.toList
      <$> Vector.take limit
      <$> Vector.map (\row -> Document (toPhyloDate  (csv'_publication_year row) (csv'_publication_month row) (csv'_publication_day row) time)
                                       (toPhyloDate' (csv'_publication_year row) (csv'_publication_month row) (csv'_publication_day row))
                                       (termsInText patterns $ (csv'_title row) <> " " <> (csv'_abstract row))
                                       (Just $ csv'_weight row)
                                       [csv'_source row]
                     ) <$> snd <$> Csv.readWeightedCsv path


-- To parse a file into a list of Document
fileToDocs' :: CorpusParser -> FilePath -> TimeUnit -> TermList -> IO [Document]
fileToDocs' parser path time lst = do
  let patterns = buildPatterns lst
  case parser of 
      Wos limit  -> wosToDocs limit  patterns time path
      Csv  _     -> csvToDocs parser patterns time path
      Csv' _     -> csvToDocs parser patterns time path


---------------
-- | Label | --
---------------


-- Config time parameters to label
timeToLabel :: Config -> [Char]
timeToLabel config = case (timeUnit config) of
      Year  p s f -> ("time_years" <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Month p s f -> ("time_months"<> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Week  p s f -> ("time_weeks" <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Day   p s f -> ("time_days"  <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))


seaToLabel :: Config -> [Char]
seaToLabel config = case (seaElevation config) of
      Constante start step   -> ("sea_cst_"  <> (show start) <> "_" <> (show step))
      Adaptative granularity -> ("sea_adapt" <> (show granularity))


sensToLabel :: Config -> [Char]
sensToLabel config = case (phyloProximity config) of
      Hamming -> undefined
      WeightedLogJaccard s -> ("WeightedLogJaccard_"  <> show s)     
      WeightedLogSim s -> ( "WeightedLogSim-sens_"  <> show s)


cliqueToLabel :: Config -> [Char]
cliqueToLabel config = case (clique config) of
      Fis s s' -> "fis_" <> (show s) <> "_" <> (show s')
      MaxClique s t f ->  "clique_" <> (show s)<> "_"  <> (show f)<> "_"  <> (show t)


syncToLabel :: Config -> [Char]
syncToLabel config = case (phyloSynchrony config) of
      ByProximityThreshold scl sync_sens scope _ -> ("scale_" <> (show scope) <> "_" <> (show sync_sens)  <> "_"  <> (show scl))
      ByProximityDistribution _ _ -> undefined

qualToConfig :: Config -> [Char]
qualToConfig config = case (phyloQuality config) of
      Quality g m -> "quality_" <> (show g) <> "_" <> (show m)     


-- To set up the export file's label from the configuration
configToLabel :: Config -> [Char]
configToLabel config = outputPath config
                    <> (unpack $ phyloName config)
                    <> "-" <> (timeToLabel config)
                    <> "-scale_" <> (show (phyloLevel config))
                    <> "-" <> (seaToLabel config)
                    <> "-" <> (sensToLabel config)
                    <> "-" <> (cliqueToLabel config)
                    <> "-level_" <> (show (_qua_granularity $ phyloQuality config))
                    <> "-" <> (syncToLabel config)
                    <> ".dot"


-- To write a sha256 from a set of config's parameters
configToSha :: PhyloStage -> Config -> [Char]
configToSha stage config = unpack 
                         $ replace "/" "-" 
                         $ T.pack (show (hash $ C8.pack label))
  where 
    label :: [Char]
    label = case stage of
      PhyloWithCliques -> (corpusPath    config)
                       <> (listPath      config)
                       <> (timeToLabel   config)
                       <> (cliqueToLabel config)
      PhyloWithLinks   -> (corpusPath    config)
                       <> (listPath      config)
                       <> (timeToLabel   config)
                       <> (cliqueToLabel config)
                       <> (sensToLabel   config)
                       <> (seaToLabel    config)
                       <> (syncToLabel   config)
                       <> (qualToConfig  config)
                       <> (show (phyloLevel config))


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


--------------
-- | Main | --
--------------   


main :: IO ()
main = do

    printIOMsg "Starting the reconstruction"

    printIOMsg "Read the configuration file"
    [args]   <- getArgs
    jsonArgs <- (eitherDecode <$> readJson args) :: IO (Either String Config)

    case jsonArgs of
        Left err     -> putStrLn err
        Right config -> do

            printIOMsg "Parse the corpus"
            mapList <- csvMapTermList (listPath config)
            corpus  <- fileToDocs' (corpusParser config) (corpusPath config) (timeUnit config) mapList
            printIOComment (show (length corpus) <> " parsed docs from the corpus")

            printIOMsg "Reconstruct the phylo"

            let phyloWithCliquesFile =  (outputPath config) <> "phyloWithCliques_" <> (configToSha PhyloWithCliques config) <> ".json"
            let phyloWithLinksFile   =  (outputPath config) <> "phyloWithLinks_"   <> (configToSha PhyloWithLinks   config) <> ".json"

            phyloWithCliquesExists <- doesFileExist phyloWithCliquesFile
            phyloWithLinksExists   <- doesFileExist phyloWithLinksFile

            -- phyloStep <- if phyloWithCliquesExists
            --                   then do
            --                     printIOMsg "Reconstruct the phylo step from an existing file"
            --                     readPhylo phyloWithCliquesFile
            --                   else do
            --                     printIOMsg "Reconstruct the phylo step from scratch"
            --                     pure $ toPhyloStep corpus mapList config

            -- writePhylo phyloWithCliquesFile phyloStep

            -- let phylo = toPhylo (setConfig config phyloStep)

            phyloWithLinks <- if phyloWithLinksExists
                                  then do 
                                    printIOMsg "Reconstruct the phylo from an existing file with intertemporal links"
                                    readPhylo phyloWithLinksFile
                                  else do 
                                    if phyloWithCliquesExists
                                      then do 
                                        printIOMsg "Reconstruct the phylo from an existing file with cliques"
                                        phyloWithCliques <- readPhylo phyloWithCliquesFile
                                        writePhylo phyloWithCliquesFile phyloWithCliques
                                        pure $ toPhylo (setConfig config phyloWithCliques)
                                      else do 
                                        printIOMsg "Reconstruct the phylo from scratch"
                                        phyloWithCliques <- pure $ toPhyloStep corpus mapList config
                                        writePhylo phyloWithCliquesFile phyloWithCliques
                                        pure $ toPhylo (setConfig config phyloWithCliques)

            writePhylo phyloWithLinksFile phyloWithLinks                                        


            -- probes

            -- writeFile ((outputPath config) <> (unpack $ phyloName config) <> "_synchronic_distance_cumu_jaccard.txt") 
            --          $ synchronicDistance' phylo 1

            -- writeFile ((outputPath config) <> (unpack $ phyloName config) <> "_inflexion_points.txt") 
            --         $ inflexionPoints phylo 1                    

            printIOMsg "End of reconstruction, start the export"

            let dot = toPhyloExport (setConfig config phyloWithLinks) 
                  
            let output = configToLabel config

            dotToFile output dot
