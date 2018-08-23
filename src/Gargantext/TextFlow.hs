{-|
Module      : Gargantext.TextFlow
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

From text to viz, all the flow of texts in Gargantext.

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}

module Gargantext.TextFlow
  where

import GHC.IO (FilePath)
import qualified Data.Text as T
import Data.Text.IO (readFile)


import qualified Data.Array.Accelerate as A
import qualified Data.Map.Strict as M
----------------------------------------------
import Gargantext.Core (Lang)
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map)
import Gargantext.Viz.Graph.Distances.Matrice (distributional)
import Gargantext.Viz.Graph (Graph(..), data2graph)
import Gargantext.Text.Metrics.Count (cooc)
import Gargantext.Text.Metrics (filterCooc, FilterConfig(..), Clusters(..), SampleBins(..), DefaultValue(..), MapListSize(..), InclusionSize(..))
import Gargantext.Text.Terms (TermType, extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Gargantext.Text.Parsers.CSV

import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)

{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/
-}


data TextFlow = CSV FilePath
              | FullText FilePath
              | Contexts [T.Text]
              | SQL Int
              | Database T.Text
                -- ExtDatabase Query
                -- IntDatabase NodeId

textFlow :: TermType Lang -> TextFlow -> IO Graph
textFlow termType workType = do
  contexts <- case workType of
                FullText path -> splitBy (Sentences 5) <$> readFile path
                CSV      path -> readCsvOn [csv_title, csv_abstract] path
                Contexts ctxt -> pure ctxt
                _             -> undefined

  textFlow' termType contexts


textFlow' :: TermType Lang -> [T.Text] -> IO Graph
textFlow' termType contexts = do
  -- Context :: Text -> [Text]
  -- Contexts = Paragraphs n | Sentences n | Chars n

  myterms <- extractTerms termType contexts
  -- TermsType = Mono | Multi | MonoMulti
  -- myterms # filter (\t -> not . elem t stopList)
  --         # groupBy (Stem|GroupList|Ontology)
  printDebug "myterms" (sum $ map length myterms)

  -- Bulding the map list
  -- compute copresences of terms, i.e. cooccurrences of terms in same context of text
  -- Cooc = Map (Term, Term) Int
  let myCooc1 = cooc myterms
  printDebug "myCooc1" (M.size myCooc1)

  -- Remove Apax: appears one time only => lighting the matrix
  let myCooc2 = M.filter (>1) myCooc1
  printDebug "myCooc2" (M.size myCooc2)

  -- Filtering terms with inclusion/Exclusion and Specificity/Genericity scores
  let myCooc3 = filterCooc ( FilterConfig (MapListSize    100 )
                                          (InclusionSize  900 )
                                          (SampleBins      10 )
                                          (Clusters         3 )
                                          (DefaultValue     0 )
                           ) myCooc2
  printDebug "myCooc3" $ M.size myCooc3
  -- putStrLn $ show myCooc3

  -- Cooc -> Matrix
  let (ti, _) = createIndices myCooc3
  printDebug "ti" $ M.size ti

  let myCooc4 = toIndex ti myCooc3
  printDebug "myCooc4" $ M.size myCooc4

  let matCooc = map2mat (0) (M.size ti) myCooc4
  printDebug "matCooc" matCooc
  
  -- Matrix -> Clustering
  --let distanceMat = conditional' matCooc
  let distanceMat = distributional matCooc
  printDebug "distanceMat" $ A.arrayShape distanceMat
  printDebug "distanceMat" distanceMat
--
  let distanceMap = mat2map distanceMat
  printDebug "distanceMap" $ M.size distanceMap

--  let distance = fromIndex fi distanceMap
--  printDebug "distance" $ M.size distance

  partitions <- cLouvain distanceMap
-- Building : -> Graph -> JSON
  printDebug "partitions" $ length partitions
  --printDebug "partitions" partitions
  pure $ data2graph (M.toList ti) myCooc4 distanceMap partitions


