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

import Control.Monad.IO.Class (MonadIO)

import Data.Map.Strict (Map)
import qualified Data.Array.Accelerate as A
import qualified Data.Map.Strict as M
----------------------------------------------
import Gargantext.Core (Lang)
import Gargantext.Core.Types (Label)
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map)
import Gargantext.Viz.Graph.Distances.Matrice (conditional)
import Gargantext.Viz.Graph (Graph(..), Node(..), Edge(..), Attributes(..), TypeNode(..))
import Gargantext.Text.Metrics.Count (cooc)
import Gargantext.Text.Metrics
import Gargantext.Text.Terms (TermType, extractTerms)
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

printDebug :: (Show a, MonadIO m) => [Char] -> a -> m ()
printDebug msg x = putStrLn $ msg <> " " <> show x
-- printDebug _ _ = pure ()

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

  -- Filtering terms with inclusion/Exclusion and Specifity/Genericity scores
  let myCooc3 = filterCooc ( FilterConfig (MapListSize    100 )
                                          (InclusionSize  400 )
                                          (SampleBins      10 )
                                          (Clusters         3 )
                                          (DefaultValue     0 )
                           ) myCooc2
  printDebug "myCooc3" $ M.size myCooc3
  putStrLn $ show myCooc3

  -- Cooc -> Matrix
  let (ti, _) = createIndices myCooc3
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
    nodes = [ Node { node_size = maybe 0 identity (M.lookup (n,n) coocs)
                   , node_type = Terms -- or Unknown
                   , node_id = cs (show n)
                   , node_label = T.unwords l
                   , node_attributes = 
                     Attributes { clust_default = maybe 0 identity 
                                (M.lookup n community_id_by_node_id) } }
            | (l, n) <- labels ]
    edges = [ Edge { edge_source = cs (show s)
                   , edge_target = cs (show t)
                   , edge_weight = w
                   , edge_id     = cs (show i) }
            | (i, ((s,t), w)) <- zip ([0..]::[Integer]) (M.toList distance) ]
-----------------------------------------------------------

