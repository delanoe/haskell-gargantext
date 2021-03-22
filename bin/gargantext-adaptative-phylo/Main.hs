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

import Data.Aeson
-- import Data.ByteString.Lazy (ByteString)
-- import Data.Maybe (isJust, fromJust)
import Data.List  (concat, nub, isSuffixOf)
import Data.String (String)
import Data.Text  (Text, unwords, unpack)
import Crypto.Hash.SHA256 (hash)
-- import Data.Digest.Pure.SHA

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year)
import Gargantext.Core.Text.Corpus.Parsers (FileFormat(..),parseFile)
import Gargantext.Core.Text.List.CSV (csvMapTermList)
import Gargantext.Core.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Core.Viz.AdaptativePhylo
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo)
import Gargantext.Core.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment)
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
-- import Gargantext.Core.Viz.Phylo.SynchronicClustering (synchronicDistance')

import GHC.IO (FilePath) 
import Prelude (Either(..))
import System.Environment
import System.Directory (listDirectory)
import Control.Concurrent.Async (mapConcurrently)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Vector
import qualified Gargantext.Core.Text.Corpus.Parsers.CSV as Csv



---------------
-- | Tools | --
---------------


-- | To get all the files in a directory or just a file
getFilesFromPath :: FilePath -> IO([FilePath])
getFilesFromPath path = do 
  if (isSuffixOf "/" path) 
    then (listDirectory path) 
    else return [path]


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
filterTerms :: Patterns -> (a, Text) -> (a, [Text])
filterTerms patterns (y,d) = (y,termsInText patterns d)
  where
    --------------------------------------
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = nub $ concat $ map (map unwords) $ extractTermsWithList pats txt
    --------------------------------------


-- | To transform a Wos file (or [file]) into a readable corpus
wosToCorpus :: Int -> FilePath -> IO ([(Int,Text)])
wosToCorpus limit path = do 
      files <- getFilesFromPath path
      take limit
        <$> map (\d -> let date' = fromJust $ _hd_publication_year d
                           title = fromJust $ _hd_title d
                           abstr = if (isJust $ _hd_abstract d)
                                   then fromJust $ _hd_abstract d
                                   else ""
                        in (date', title <> " " <> abstr)) 
        <$> concat 
        <$> mapConcurrently (\file -> 
              filter (\d -> (isJust $ _hd_publication_year d)
                         && (isJust $ _hd_title d))
                <$> parseFile WOS (path <> file) ) files


-- | To transform a Csv file into a readable corpus
csvToCorpus :: Int -> FilePath -> IO ([(Int,Text)])
csvToCorpus limit path = Vector.toList
    <$> Vector.take limit
    <$> Vector.map (\row -> (csv_publication_year row, (csv_title row) <> " " <> (csv_abstract row)))
    <$> snd <$> Csv.readFile path


-- | To use the correct parser given a CorpusType
fileToCorpus :: CorpusParser -> FilePath -> IO ([(Int,Text)])
fileToCorpus parser path = case parser of 
  Wos limit -> wosToCorpus limit path
  Csv limit -> csvToCorpus limit path


-- | To parse a file into a list of Document
fileToDocs :: CorpusParser -> FilePath -> TermList -> IO [Document]
fileToDocs parser path lst = do
  corpus <- fileToCorpus parser path
  let patterns = buildPatterns lst
  pure $ map ( (\(y,t) -> Document y t) . filterTerms patterns) corpus


-- Config time parameters to label
timeToLabel :: Config -> [Char]
timeToLabel config = case (timeUnit config) of
      Year p s f -> ("time"<> "_"<> (show p) <> "_"<> (show s) <> (show f))


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

configToSha :: Config -> [Char]
configToSha config = show (hash $ C8.pack label)
  where 
    label :: [Char]
    label = (corpusPath    config)
         <> (listPath      config)
         <> (timeToLabel   config)
         <> (cliqueToLabel config)
         <> (sensToLabel   config)

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
            corpus  <- fileToDocs (corpusParser config) (corpusPath config) mapList
            printIOComment (show (length corpus) <> " parsed docs from the corpus")

            printIOMsg "Reconstruct the Phylo"
            
            let phylo = toPhylo corpus mapList config

            -- | probes

            -- writeFile ((outputPath config) <> (unpack $ phyloName config) <> "_synchronic_distance_cumu_jaccard.txt") 
            --          $ synchronicDistance' phylo 1

            -- writeFile ((outputPath config) <> (unpack $ phyloName config) <> "_inflexion_points.txt") 
            --         $ inflexionPoints phylo 1                    

            printIOMsg "End of reconstruction, start the export"

            let dot = toPhyloExport phylo 
                  
            let output = configToLabel config

            dotToFile output dot
