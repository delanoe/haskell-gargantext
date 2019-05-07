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
import Data.Text (Text, unwords)
import GHC.Generics
import GHC.IO (FilePath)
import Gargantext.Prelude
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.Parsers (FileFormat(..),parseDocs)
import Gargantext.Text.Terms.WithList
import Gargantext.Text.Context (TermList)

import System.Environment

import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Viz.Phylo.View.Export
import Gargantext.Viz.Phylo.View.ViewMaker

import qualified Data.Map    as DM
import qualified Data.Vector as DV
import qualified Data.List   as DL
import qualified Data.Text   as DT
import qualified Prelude     as P
import qualified Data.ByteString.Lazy as L


--------------
-- | Conf | --
--------------


type ListPath   = FilePath
type CorpusPath = FilePath
data CorpusType = Wos | Csv deriving (Show,Generic) 
type Limit = Int

data Conf = 
     Conf { corpusPath :: CorpusPath
          , corpusType :: CorpusType
          , listPath   :: ListPath
          , outputPath :: FilePath
          , phyloName  :: Text
          , limit      :: Limit
     } deriving (Show,Generic)

instance FromJSON Conf
instance ToJSON Conf

instance FromJSON CorpusType
instance ToJSON CorpusType

-- | Get the conf from a Json file
getJson :: FilePath -> IO L.ByteString
getJson path = L.readFile path


---------------
-- | Parse | --
---------------


filterTerms :: Patterns -> (a, Text) -> (a, [Text])
filterTerms patterns (year', doc) = (year',termsInText patterns doc)
  where
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = DL.nub $ DL.concat $ map (map unwords) $ extractTermsWithList pats txt


csvToCorpus :: Int -> CorpusPath -> IO ([(Int,Text)])
csvToCorpus limit csv = DV.toList
                      . DV.take limit
                      . DV.map (\n -> (csv_publication_year n, (csv_title n) <> " " <> (csv_abstract n)))
                      . snd <$> readCsv csv


wosToCorpus :: Int -> CorpusPath -> IO ([(Int,Text)])
wosToCorpus limit path = undefined


fileToCorpus :: CorpusType -> Int -> CorpusPath -> IO ([(Int,Text)])
fileToCorpus format limit path = case format of 
  Wos -> wosToCorpus limit path
  Csv -> csvToCorpus limit path


parse :: Limit -> CorpusPath -> TermList -> IO [Document]
parse limit corpus lst = do
  corpus' <- csvToCorpus limit corpus
  let patterns = buildPatterns lst
  pure $ map ( (\(y,t) -> Document y t) . filterTerms patterns) corpus'


--------------
-- | Main | --
--------------


main :: IO ()
main = do 

  putStrLn $ show "--| Read the conf |--"

  [jsonPath] <- getArgs

  confJson <- (eitherDecode <$> getJson jsonPath) :: IO (P.Either P.String Conf)

  case confJson of
    P.Left err -> putStrLn err
    P.Right conf -> do  

      putStrLn $ show "--| Parse the corpus |--"

      termList <- csvGraphTermList (listPath conf)

      corpus <- parse (limit conf) (corpusPath conf) termList

      let roots = DL.nub $ DL.concat $ map text corpus

      putStrLn $ show "--| Build the phylo |--" 
      
      let query = PhyloQueryBuild (phyloName conf) "" 5 3 defaultFis [] [] (WeightedLogJaccard $ WLJParams 0.00001 10) 2 (RelatedComponents $ RCParams $ WeightedLogJaccard $ WLJParams 0.5 10)

      let queryView = PhyloQueryView 2 Merge False 1 [BranchAge] [defaultSmallBranch] [BranchPeakFreq,GroupLabelCooc] (Just (ByBranchAge,Asc)) Json Flat True           

      let phylo = toPhylo query corpus roots termList

      let view  = toPhyloView queryView phylo

      putStrLn $ show "--| Export the phylo as a dot graph |--" 

      let outputFile = (outputPath conf) P.++ (DT.unpack $ phyloName conf) P.++ ".dot"

      P.writeFile outputFile $ dotToString $ viewToDot view       
