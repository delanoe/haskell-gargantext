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
import Data.Maybe ()
import Data.List  (concat, nub)
import Data.String (String)
import Data.Text  (Text, unwords)

import Gargantext.Prelude
import Gargantext.Text.Context (TermList)
import Gargantext.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Viz.AdaptativePhylo

import GHC.IO (FilePath) 
import Prelude (Either(..))
import System.Environment

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Vector
import qualified Gargantext.Text.Corpus.Parsers.CSV as Csv


---------------
-- | Tools | --
---------------


-- | To print an important message as an IO()
printIOMsg :: String -> IO ()
printIOMsg msg = 
    putStrLn ( "\n"
            <> "------------" 
            <> "\n"
            <> "-- | " <> msg <> "\n" )


-- | To print a comment as an IO()
printIOComment :: String -> IO ()
printIOComment cmt =
    putStrLn ( "\n" <> cmt <> "\n" )


-- | To read and decode a Json file
readJson :: FilePath -> IO ByteString
readJson path = Lazy.readFile path

-- | To filter the Ngrams of a document based on the termList
filterTerms :: Patterns -> (a, Text) -> (a, [Text])
filterTerms patterns (y,d) = (y,termsInText patterns d)
  where
    --------------------------------------
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = nub $ concat $ map (map unwords) $ extractTermsWithList pats txt
    --------------------------------------


-- | To transform a Csv nfile into a readable corpus
csvToCorpus :: Int -> FilePath -> IO ([(Int,Text)])
csvToCorpus limit path = Vector.toList
    <$> Vector.take limit
    <$> Vector.map (\row -> (csv_publication_year row, (csv_title row) <> " " <> (csv_abstract row)))
    <$> snd <$> Csv.readFile path


-- | To use the correct parser given a CorpusType
fileToCorpus :: CorpusParser -> Int -> FilePath -> IO ([(Int,Text)])
fileToCorpus parser limit path = case parser of 
  -- To do Wos from legacy Main.hs
  Wos -> undefined
  Csv -> csvToCorpus limit path


-- | To parse a file into a list of Document
fileToDocs :: CorpusParser -> Int -> FilePath -> TermList -> IO [Document]
fileToDocs parser limit path lst = do
  corpus <- fileToCorpus parser limit path
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
            corpus  <- fileToDocs (corpusParser config) (corpusLimit config) (corpusPath config) mapList
            printIOComment (show (length corpus) <> " parsed docs from the corpus")

    