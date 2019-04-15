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
import Data.ByteString.Lazy (writeFile)
import Data.Text (Text)
import GHC.Generics
import Gargantext.Prelude
import Gargantext.Text.List.CSV (csvGraphTermList)
import Gargantext.Text.Parsers.CSV (readCsv, csv_title, csv_abstract, csv_publication_year)
import Gargantext.Text.Terms.WithList
import System.Environment
import qualified Data.Map    as DM
import qualified Data.Vector as DV

------------------------------------------------------------------------
-- Format to produce the Phylo
data TextsByYear =
  TextsByYear { year    :: Int
              , texts   :: [[Text]]
              } deriving (Show, Generic)

instance ToJSON TextsByYear
------------------------------------------------------------------------

filterTerms :: Patterns -> (a, [Text]) -> (a, [[Text]])
filterTerms patterns (year', docs) = (year', map (termsInText patterns) docs)
  where
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = extractTermsWithList' pats txt

main :: IO ()
main = do
  [corpusFile, termListFile, outputFile] <- getArgs

  corpus <- DM.fromListWith (<>)
                             . DV.toList
                             . DV.map (\n -> (csv_publication_year n, [(csv_title n) <> " " <> (csv_abstract n)]))
                             . snd
                           <$> readCsv corpusFile

  termList <- csvGraphTermList termListFile
  putStrLn $ show $ length termList
  let patterns = buildPatterns termList

  let corpusParsed = map ( (\(y,t) -> TextsByYear y t) . filterTerms patterns) (DM.toList corpus)

  -- TODO Phylo here
  writeFile outputFile $ encode corpusParsed

