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

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Strict             #-}

module Main where

import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Monad (mapM)
import Data.Aeson
import Data.List ((++),concat)
import Data.Maybe
import Data.Text (Text, unwords, unlines)
import GHC.Generics
import GHC.IO (FilePath)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import Gargantext.Text.Context (TermList)
import Gargantext.Text.Corpus.Parsers (FileFormat(..),parseFile)
import Gargantext.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Terms.WithList
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.View.Export
import Gargantext.Viz.Phylo.View.ViewMaker
import System.Directory (doesFileExist)
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.List   as DL
import qualified Data.Map    as DM
import qualified Data.Text   as DT
import qualified Data.Vector as DV
import qualified Gargantext.Text.Corpus.Parsers.CSV as CSV
import qualified Prelude     as P


--------------
-- | Conf | --
--------------


type ListPath   = FilePath
type FisPath    = FilePath
type CorpusPath = FilePath
data CorpusType = Wos | Csv deriving (Show,Generic) 
type Limit = Int

data Conf = 
     Conf { corpusPath  :: CorpusPath
          , corpusType  :: CorpusType
          , listPath    :: ListPath
          , fisPath     :: FilePath
          , outputPath  :: FilePath
          , phyloName   :: Text
          , limit       :: Limit
          , timeGrain   :: Int
          , timeStep    :: Int
          , timeFrame   :: Int
          , timeFrameTh :: Double          
          , timeTh      :: Double
          , timeSens    :: Double
          , reBranchThr :: Double
          , reBranchNth :: Int
          , clusterTh   :: Double
          , clusterSens :: Double
          , phyloLevel  :: Int
          , viewLevel   :: Int
          , fisSupport  :: Int
          , fisClique   :: Int
          , minSizeBranch :: Int 
     } deriving (Show,Generic)

instance FromJSON Conf
instance ToJSON Conf

instance FromJSON CorpusType
instance ToJSON CorpusType


decoder :: P.Either a b -> b
decoder (P.Left _) = P.error "Error"
decoder (P.Right x) = x

-- | Get the conf from a Json file
getJson :: FilePath -> IO L.ByteString
getJson path = L.readFile path


---------------
-- | Parse | --
---------------


-- | To filter the Ngrams of a document based on the termList
filterTerms :: Patterns -> (a, Text) -> (a, [Text])
filterTerms patterns (y,d) = (y,termsInText patterns d)
  where
    --------------------------------------
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = DL.nub
                         $ DL.concat
                         $ map (map unwords)
                         $ extractTermsWithList pats txt
    --------------------------------------


-- | To transform a Csv nfile into a readable corpus
csvToCorpus :: Limit -> CorpusPath -> IO ([(Int,Text)])
csvToCorpus limit csv = DV.toList
                      -- . DV.reverse
                      . DV.take limit
                      -- . DV.reverse
                      . DV.map (\n -> (csv_publication_year n, (csv_title n) <> " " <> (csv_abstract n)))
                      . snd <$> CSV.readFile csv


-- | To transform a Wos nfile into a readable corpus
wosToCorpus :: Limit -> CorpusPath -> IO ([(Int,Text)])
wosToCorpus limit path = DL.take limit
                         . map (\d -> ((fromJust $_hyperdataDocument_publication_year d)
                                    ,(fromJust $_hyperdataDocument_title d) <> " " <> (fromJust $_hyperdataDocument_abstract d)))
                         . filter (\d -> (isJust $_hyperdataDocument_publication_year d)
                                      && (isJust $_hyperdataDocument_title d)
                                      && (isJust $_hyperdataDocument_abstract d))
                         . concat
                         <$> mapConcurrently (\idx -> parseFile WOS (path <> show(idx) <> ".txt")) [1..20]


-- | To use the correct parser given a CorpusType
fileToCorpus :: CorpusType -> Limit -> CorpusPath -> IO ([(Int,Text)])
fileToCorpus format limit path = case format of 
  Wos -> wosToCorpus limit path
  Csv -> csvToCorpus limit path


-- | To parse a file into a list of Document
parse :: CorpusType -> Limit -> CorpusPath -> TermList -> IO [Document]
parse format limit path l = do
  corpus <- fileToCorpus format limit path
  let patterns = buildPatterns l
  pure $ map ( (\(y,t) -> Document y t) . filterTerms patterns) corpus


-- | To parse an existing Fis file
parseFis :: FisPath -> Text -> Int -> Int -> Int -> Int -> IO [PhyloFis]
parseFis path name grain step support clique = do
  fisExists <- doesFileExist (path <> (DT.unpack name) <> "_" <> show(grain) <> "_" <> show(step) <> "_" <> show(support) <> "_" <> show(clique) <> ".json")
  if fisExists 
    then do 
      fisJson <- (eitherDecode <$> getJson (path <> (DT.unpack name) <> "_" <> show(grain) <> "_" <> show(step) <> "_" <> show(support) <> "_" <> show(clique) <> ".json")) :: IO (P.Either P.String [PhyloFis])
      case fisJson of 
        P.Left err -> do 
                       putStrLn err
                       pure []
        P.Right fis -> pure fis
    else pure []

writeFis :: FisPath -> Text -> Int -> Int -> Int -> Int -> DM.Map (Date,Date) [PhyloFis] -> IO ()
writeFis path name grain step support clique fis = do 
  let fisPath = path <> (DT.unpack name) <> "_" <> show(grain) <> "_" <> show(step) <> "_" <> show(support) <> "_" <> show(clique) <> ".json"
  L.writeFile fisPath $ encode (DL.concat $ DM.elems fis)

--------------
-- | Main | --
--------------


main :: IO ()
main = do 

  [jsonPath] <- getArgs

  confJson <- (eitherDecode <$> getJson jsonPath) :: IO (P.Either P.String Conf)

  case confJson of
    P.Left err -> putStrLn err
    P.Right conf -> do

      termList <- csvGraphTermList (listPath conf)

      corpus <- parse (corpusType conf) (limit conf) (corpusPath conf) termList

      putStrLn $ ("\n" <> show (length corpus) <> " parsed docs") 

      fis <- parseFis (fisPath conf) (phyloName conf) (timeGrain conf) (timeStep conf) (fisSupport conf) (fisClique conf)

      putStrLn $ ("\n" <> show (length fis) <> " parsed fis")

      let fis'  = DM.fromListWith (++) $ DL.sortOn (fst . fst) $ map (\f -> (getFisPeriod f,[f])) fis    
      
      let query = PhyloQueryBuild (phyloName conf) "" (timeGrain conf) (timeStep conf) 
                  (Fis $ FisParams True (fisSupport conf) (fisClique conf)) [] [] (WeightedLogJaccard $ WLJParams (timeTh conf) (timeSens conf)) (timeFrame conf) (timeFrameTh conf) 
                  (reBranchThr conf) (reBranchNth conf) (phyloLevel conf)
                  (RelatedComponents $ RCParams $ WeightedLogJaccard $ WLJParams (clusterTh conf) (clusterSens conf))

      let queryView = PhyloQueryView (viewLevel conf) Merge False 1 [BranchAge,BranchBirth,BranchGroups] [SizeBranch $ SBParams (minSizeBranch conf)] [GroupLabelIncDyn,BranchPeakInc] (Just (ByBranchBirth,Asc)) Json Flat True           

      let phylo = toPhylo query corpus termList fis'

      writeFis (fisPath conf) (phyloName conf) (timeGrain conf) (timeStep conf) (fisSupport conf) (fisClique conf) (getPhyloFis phylo)

      let view  = toPhyloView queryView phylo

      putStrLn $ ("phylo completed until level " <> show (phyloLevel conf) <> ", export at level " <> show (viewLevel conf)) 

      let outputFile = (outputPath conf) <> (DT.unpack $ phyloName conf)
                                         <> "_" <> show (limit conf) <> "_"
                                         <> "_" <> show (timeTh conf) <> "_"
                                         <> "_" <> show (timeSens conf) <> "_"
                                         <> "_" <> show (clusterTh conf) <> "_"
                                         <> "_" <> show (clusterSens conf) 
                                         <> ".dot"

      P.writeFile outputFile $ dotToString $ viewToDot view      
