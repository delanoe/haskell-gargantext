{-|
Module      : Gargantext.Text.Flow
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
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Text.Flow
  where

--import qualified Data.Array.Accelerate as A
--import qualified Data.Set as DS
import Control.Monad.Reader
import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO (readFile)
import Database.PostgreSQL.Simple (Connection)
import GHC.IO (FilePath)
import Gargantext.Core (Lang)
import Gargantext.Core.Types (CorpusId)
import Gargantext.Database.Schema.Node
import Gargantext.Database.Types.Node
import Gargantext.Prelude
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))
import Gargantext.Text.Metrics (filterCooc, FilterConfig(..), Clusters(..), SampleBins(..), DefaultValue(..), MapListSize(..), InclusionSize(..))
import Gargantext.Text.Metrics.Count (cooc)
import Gargantext.Text.Parsers.CSV
import Gargantext.Text.Terms (TermType, extractTerms)
import Gargantext.Viz.Graph (Graph(..), data2graph)
import Gargantext.Viz.Graph.Bridgeness (bridgeness)
import Gargantext.Viz.Graph.Distances.Matrice (measureConditional)
import Gargantext.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map)
{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/
-}


contextText :: [T.Text]
contextText = ["The dog is an animal."
              ,"The bird is an animal."
              ,"The dog is an animal."
              ,"The animal is a bird or a dog ?"
              ,"The table is an object."
              ,"The pen is an object."
              ,"The object is a pen or a table ?"
              ,"The girl is a human."
              ,"The boy  is a human."
              ,"The boy or the girl are human."
              ]


-- | Control the flow of text
data TextFlow = CSV FilePath
              | FullText FilePath
              | Contexts [T.Text]
              | DBV3 Connection CorpusId
              | Query T.Text


textFlow :: TermType Lang -> TextFlow -> IO Graph
textFlow termType workType = do
  contexts <- case workType of
                FullText path -> splitBy (Sentences 5) <$> readFile path
                CSV      path -> readCsvOn [csv_title, csv_abstract] path
                Contexts ctxt -> pure ctxt
                DBV3 con corpusId -> catMaybes <$> map (\n -> hyperdataDocumentV3_title (_node_hyperdata n)  <> hyperdataDocumentV3_abstract (_node_hyperdata n))<$> runReaderT (getDocumentsV3WithParentId corpusId) con
                _             -> undefined -- TODO Query not supported

  textFlow' termType contexts


textFlow' :: TermType Lang -> [T.Text] -> IO Graph
textFlow' termType contexts = do
  -- Context :: Text -> [Text]
  -- Contexts = Paragraphs n | Sentences n | Chars n

  myterms <- extractTerms termType contexts
  -- TermsType = Mono | Multi | MonoMulti
  -- myterms # filter (\t -> not . elem t stopList)
  --         # groupBy (Stem|GroupList|Ontology)
  --printDebug "terms" myterms
  --printDebug "myterms" (sum $ map length myterms)

  -- Bulding the map list
  -- compute copresences of terms, i.e. cooccurrences of terms in same context of text
  -- Cooc = Map (Term, Term) Int
  let myCooc1 = cooc myterms
  --printDebug "myCooc1 size" (M.size myCooc1)

  -- Remove Apax: appears one time only => lighting the matrix
  let myCooc2 = M.filter (>0) myCooc1
  --printDebug "myCooc2 size" (M.size myCooc2)
  --printDebug "myCooc2" myCooc2
  g <- cooc2graph myCooc2
  pure g

-- TODO use Text only here instead of [Text]
cooc2graph :: (Map ([Text], [Text]) Int) -> IO Graph
cooc2graph myCooc = do
  --printDebug "myCooc" myCooc
  -- Filtering terms with inclusion/Exclusion and Specificity/Genericity scores
  let myCooc3 = filterCooc ( FilterConfig (MapListSize    350 )
                                          (InclusionSize  500 )
                                          (SampleBins      10 )
                                          (Clusters         3 )
                                          (DefaultValue     0 )
                           ) myCooc
  --printDebug "myCooc3 size" $ M.size myCooc3
  --printDebug "myCooc3" myCooc3

  -- Cooc -> Matrix
  let (ti, _) = createIndices myCooc3
  --printDebug "ti size" $ M.size ti
  --printDebug "ti" ti

  let myCooc4 = toIndex ti myCooc3
  --printDebug "myCooc4 size" $ M.size myCooc4
  --printDebug "myCooc4" myCooc4

  let matCooc = map2mat (0) (M.size ti) myCooc4
  --printDebug "matCooc shape" $ A.arrayShape matCooc
  --printDebug "matCooc" matCooc

  -- Matrix -> Clustering
  let distanceMat = measureConditional matCooc
  --let distanceMat = distributional matCooc
  --printDebug "distanceMat shape" $ A.arrayShape distanceMat
  --printDebug "distanceMat" distanceMat

  --let distanceMap = M.filter (>0) $ mat2map distanceMat
  let distanceMap = M.map (\_ -> 1) $ M.filter (>0) $ mat2map distanceMat
  --printDebug "distanceMap size" $ M.size distanceMap
  --printDebug "distanceMap" distanceMap

--  let distance = fromIndex fi distanceMap
  --printDebug "distance" $ M.size distance
  
  partitions <- case M.size distanceMap > 0 of
                  True  -> cLouvain distanceMap
                  False -> panic "Text.Flow: DistanceMap is empty"
-- Building : -> Graph -> JSON
  --printDebug "partitions" $ DS.size $ DS.fromList $ map (l_community_id) partitions
  --printDebug "partitions" partitions
  let distanceMap' = bridgeness 300 partitions distanceMap
  pure $ data2graph (M.toList ti) myCooc4 distanceMap' partitions

