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

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (isJust, fromJust)
import Data.List  (concat, nub, isSuffixOf, take)
import Data.String (String)
import Data.Text  (Text, unwords, unpack)

import Gargantext.Prelude
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Gargantext.Text.Context (TermList)
import Gargantext.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.Corpus.Parsers (FileFormat(..),parseFile)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloMaker  (toPhylo)
import Gargantext.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment)
import Gargantext.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
-- import Gargantext.Viz.Phylo.SynchronicClustering (synchronicDistance')

import GHC.IO (FilePath) 
import Prelude (Either(..))
import System.Environment
import System.Directory (listDirectory)
import Control.Concurrent.Async (mapConcurrently)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Vector
import qualified Gargantext.Text.Corpus.Parsers.CSV as Csv


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
readJson :: FilePath -> IO ByteString
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
        <$> map (\d -> let date' = fromJust $ _hyperdataDocument_publication_year d
                           title = fromJust $ _hyperdataDocument_title d
                           abstr = if (isJust $ _hyperdataDocument_abstract d)
                                   then fromJust $ _hyperdataDocument_abstract d
                                   else ""
                        in (date', title <> " " <> abstr)) 
        <$> concat 
        <$> mapConcurrently (\file -> 
              filter (\d -> (isJust $ _hyperdataDocument_publication_year d)
                         && (isJust $ _hyperdataDocument_title d))
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
            mapList <- csvGraphTermList (listPath config)
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

            let output = (outputPath config) 
                      <> (unpack $ phyloName config)
                      <> "_V2.dot"

            dotToFile output dot