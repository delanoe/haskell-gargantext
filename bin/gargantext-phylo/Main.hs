{-|
Module      : Main.hs
Description : Gargantext starter binary with Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Phylo binaries

 -}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE Strict             #-}

module Main where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import GHC.IO (FilePath)
import Gargantext.Prelude
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.Terms.WithList
import System.Environment

import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Viz.Phylo.View.Export
import Gargantext.Viz.Phylo.View.ViewMaker

import qualified Data.Map    as DM
import qualified Data.Vector as DV
import qualified Data.List   as DL
import qualified Prelude     as P
import qualified Data.ByteString.Lazy as L

------------------------------------------------------------------------
-- Format to produce the Phylo
data TextsByYear =
  TextsByYear { year    :: Int
              , texts   :: [[Text]]
              } deriving (Show, Generic)

instance ToJSON TextsByYear

instance ToJSON Document
------------------------------------------------------------------------

filterTerms :: Patterns -> (a, Text) -> (a, [Text])
filterTerms patterns (year', doc) = (year',termsInText patterns doc)
  where
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = extractTermsWithList' pats txt


-- csvToCorpus :: Int -> FilePath -> IO (DM.Map Int [Text])
csvToCorpus :: Int -> FilePath -> IO ([(Int,Text)])
csvToCorpus limit csv = DV.toList
                        -- DM.fromListWith (<>)
                      . DV.take limit
                      . DV.map (\n -> (csv_publication_year n, (csv_title n) <> " " <> (csv_abstract n)))
                      . snd <$> readCsv csv


main :: IO ()
main = do
  
  -- [corpusFile, termListFile, outputFile] <- getArgs

  let corpusPath   = "/home/qlobbe/data/epique/corpus/cultural_evolution/texts/fullCorpus.csv"
  let termListPath = "/home/qlobbe/data/epique/corpus/cultural_evolution/termList.csv"
  let outputPath   = "/home/qlobbe/data/epique/output/cultural_evolution.dot"

  corpus <- csvToCorpus 10 corpusPath 

  termList <- csvGraphTermList termListPath

  putStrLn $ show $ length termList
  
  let patterns = buildPatterns termList

  let corpusParsed = map ( (\(y,t) -> Document y t) . filterTerms patterns) corpus

  let query = PhyloQueryBuild "cultural_evolution" "Test" 5 3 defaultFis [] [] defaultWeightedLogJaccard 3 defaultRelatedComponents

  let tree = []

  let foundations = DL.nub $ DL.concat $ map _pat_terms patterns

  -- let phylo = toPhylo query corpusParsed foundations tree

  -- let queryView = PhyloQueryView 2 Merge False 1 [BranchAge] [defaultSmallBranch] [BranchPeakFreq,GroupLabelCooc] (Just (ByBranchAge,Asc)) Json Flat True

  -- let view = toPhyloView queryView phylo

    -- TODO Phylo here
  -- P.writeFile outputPath $ dotToString $ viewToDot view 
  L.writeFile outputPath $ encode corpusParsed



