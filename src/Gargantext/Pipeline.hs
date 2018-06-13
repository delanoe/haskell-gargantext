{-|
Module      : Gargantext.Pipeline
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}

module Gargantext.Pipeline
  where

import qualified Data.Text as T
import Data.Text.IO (readFile)

import Control.Arrow ((***))
import Data.Map.Strict (Map)
import qualified Data.Array.Accelerate as A
import qualified Data.Map.Strict as M
import qualified Data.List       as L
import Data.Tuple.Extra (both)
----------------------------------------------
import Gargantext.Core (Lang(FR))
import Gargantext.Core.Types (Label)
import Gargantext.Prelude
import Prelude (print, seq)

import Gargantext.Viz.Graph.Index (score, createIndices, toIndex, fromIndex, cooc2mat, map2mat, mat2map)
import Gargantext.Viz.Graph.Distances.Matrice (conditional', conditional, distributional)
import Gargantext.Viz.Graph.Index (Index)
import Gargantext.Viz.Graph (Graph(..), Node(..), Edge(..), Attributes(..), TypeNode(..))
import Gargantext.Text.Metrics.Count (cooc)
import Gargantext.Text.Metrics
import Gargantext.Text.Terms (TermType(Multi, Mono), extractTerms)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Gargantext.Text.Parsers.CSV

import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain, LouvainNode(..))


{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/

-}

data WorkType = CSV | FullText

-- workflow :: Lang (EN|FR) -> FilePath -> Graph
workflow termsLang workType path = do
  -- Text  <- IO Text <- FilePath
  contexts <- case workType of
                FullText -> splitBy (Sentences 5) <$> readFile path
                CSV      -> readCsvOn [csv_title, csv_abstract] path

  -- Context :: Text -> [Text]
  -- Contexts = Paragraphs n | Sentences n | Chars n

  myterms <- extractTerms (Mono FR) contexts
  -- TermsType = Mono | Multi | MonoMulti
  -- myterms # filter (\t -> not . elem t stopList)
  --         # groupBy (Stem|GroupList|Ontology)
  printDebug "myterms" (sum $ map length myterms)

  -- Bulding the map list
  -- compute copresences of terms
  -- Cooc = Map (Term, Term) Int
  let myCooc1 = cooc myterms
  printDebug "myCooc1" (M.size myCooc1)

  -- Remove Apax: appears one time only => lighting the matrix
  let myCooc2 = M.filter (>1) myCooc1
  printDebug "myCooc2" (M.size myCooc2)

  -- Filtering terms with inclusion/Exclusion and Specifity/Genericity scores
  let myCooc3 = filterCooc ( FilterConfig (MapListSize   1000 )
                                          (InclusionSize 4000 )
                                          (SampleBins      10 )
                                          (Clusters         3 )
                                          (DefaultValue     0 )
                           ) myCooc2
  printDebug "myCooc3" $ M.size myCooc3

  -- Cooc -> Matrix
  let (ti, fi) = createIndices myCooc3
  printDebug "ti" $ M.size ti

  let myCooc4 = toIndex ti myCooc3
  printDebug "myCooc4" $ M.size myCooc4

  let matCooc = map2mat (0) (M.size ti) myCooc4
  --printDebug "matCooc" matCooc
  -- Matrix -> Clustering
  let distanceMat = conditional matCooc
--  let distanceMat = distributional matCooc
  printDebug "distanceMat" $ A.arrayShape distanceMat
  --printDebug "distanceMat" distanceMat
-- 
  let distanceMap = mat2map distanceMat
  printDebug "distanceMap" $ M.size distanceMap
--{-
--  let distance = fromIndex fi distanceMap
--  printDebug "distance" $ M.size distance
---}
  partitions <- cLouvain distanceMap
------ | Building : -> Graph -> JSON
  printDebug "partitions" $ length partitions
  --printDebug "partitions" partitions
  pure $ data2graph (M.toList ti) myCooc4 distanceMap partitions


-----------------------------------------------------------
-- distance should not be a map since we just "toList" it (same as cLouvain)
data2graph :: [(Label, Int)] -> Map (Int, Int) Int
                             -> Map (Int, Int) Double
                             -> [LouvainNode]
              -> Graph
data2graph labels coocs distance partitions = Graph nodes edges
  where
    community_id_by_node_id = M.fromList [ (n, c) | LouvainNode n c <- partitions ]
    nodes = [ Node { n_size = maybe 0 identity (M.lookup (n,n) coocs)
                   , n_type = Terms -- or Unknown
                   , n_id = cs (show n)
                   , n_label = T.unwords l
                   , n_attributes = 
                     Attributes { clust_default = maybe 0 identity 
                                (M.lookup n community_id_by_node_id) } }
            | (l, n) <- labels ]
    edges = [ Edge { e_source = s
                   , e_target = t
                   , e_weight = w
                   , e_id     = i }
            | (i, ((s,t), w)) <- zip [0..] (M.toList distance) ]
-----------------------------------------------------------

printDebug msg x = putStrLn $ msg <> " " <> show x
--printDebug _ _ = pure ()




