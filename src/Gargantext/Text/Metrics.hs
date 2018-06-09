{-|
Module      : Gargantext.Text.Metrics
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly reexport functions in @Data.Text.Metrics@


TODO
noApax :: Ord a => Map a Occ -> Map a Occ
noApax m = M.filter (>1) m

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics 
  where

import Data.Text (Text, pack)
import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Tuple.Extra (both)
--import GHC.Real (Ratio)
--import qualified Data.Text.Metrics as DTM
import Data.Array.Accelerate (toList)
import Math.KMeans (kmeans, euclidSq, elements)


import Gargantext.Prelude

import Gargantext.Text.Metrics.Count (occurrences, cooc)
import Gargantext.Text.Terms (TermType(MonoMulti), terms)
import Gargantext.Core (Lang(EN))
import Gargantext.Core.Types (Terms(..))
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index

import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.Array.Accelerate as DAA

import GHC.Real (round)

filterCooc :: Ord t => Map (t, t) Int -> Map (t, t) Int
filterCooc cc = filterCooc' ts cc
  where
    ts     = map _scored_terms $ takeSome 350 5 2 $ coocScored cc

filterCooc' :: Ord t => [t] -> Map (t, t) Int -> Map (t, t) Int
filterCooc' ts m = foldl' (\m' k -> M.insert k (maybe errMessage identity $ M.lookup k m) m') M.empty selection
  where
    errMessage = panic "Filter cooc: no key"
    selection  = [(x,y) | x <- ts, y <- ts, x > y]


type MapListSize = Int
type SampleBins  = Double
type Clusters    = Int

-- | Map list creation
-- Kmeans split into (Clusters::Int) main clusters with Inclusion/Exclusion (relevance score)
-- Sample the main cluster ordered by specificity/genericity in (SampleBins::Double) parts
-- each parts is then ordered by Inclusion/Exclusion
-- take n scored terms in each parts where n * SampleBins = MapListSize.
takeSome :: Ord t => MapListSize -> SampleBins -> Clusters -> [Scored t] -> [Scored t]
takeSome l s k scores = L.take l
                    $ takeSample n m
                    $ splitKmeans k scores
  where
    -- TODO: benchmark with accelerate-example kmeans version
    splitKmeans x xs = elements
                     $ V.head
                     $ kmeans (\i -> VU.fromList ([(_scored_incExc i :: Double)]))
                              euclidSq x xs
    n = round ((fromIntegral l)/s)
    m = round $ (fromIntegral $ length scores) / (s)
    takeSample n m xs = L.concat $ map (L.take n)
                                 $ L.reverse $ map (L.sortOn _scored_incExc)
                                 -- TODO use kmeans s instead of splitEvery
                                 -- in order to split in s heteregenous parts
                                 -- without homogeneous order hypothesis
                                 $ splitEvery m 
                                 $ L.reverse $ L.sortOn _scored_speGen xs


data Scored t = Scored { _scored_terms  :: !t
                       , _scored_incExc :: !InclusionExclusion
                       , _scored_speGen :: !SpecificityGenericity
                     } deriving (Show)

coocScored :: Ord t => Map (t,t) Int -> [Scored t]
coocScored m = zipWith (\(i,t) (inc,spe) -> Scored t inc spe) (M.toList fi) scores
  where
    (ti,fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores = DAA.toList $ DAA.run $ DAA.zip (DAA.use is) (DAA.use ss)




















incExcSpeGen_sorted :: Ord t => Map (t,t) Int -> ([(t,Double)],[(t,Double)])
incExcSpeGen_sorted m = both ordonne (incExcSpeGen $ cooc2mat ti m)
  where
    (ti,fi) = createIndices m
    ordonne x = L.reverse $ L.sortOn snd $ zip (map snd $ M.toList fi) (toList x)



metrics_text :: Text
metrics_text = T.intercalate " " metrics_sentences

metrics_sentences' :: [Text]
metrics_sentences' = splitBy (Sentences 0) metrics_text

-- | Sentences 
metrics_sentences :: [Text]
metrics_sentences = [ "There is a table with a glass of wine and a spoon."
                    , "I can see the glass on the table."
                    , "There was only a spoon on that table."
                    , "The glass just fall from the table, pouring wine everywhere."
                    , "I wish the glass did not contain wine."
                    ]

metrics_sentences_Test = metrics_sentences == metrics_sentences'

-- | Terms reordered to visually check occurrences
-- >>> 
{- [ [["table"],["glass"],["wine"],["spoon"]]
   , [["glass"],["table"]]
   , [["spoon"],["table"]]
   , [["glass"],["table"],["wine"]]
   , [["glass"],["wine"]]
   ]
-}

metrics_terms :: IO [[Terms]]
metrics_terms = mapM (terms MonoMulti EN) $ splitBy (Sentences 0) metrics_text

-- | Occurrences
{-
fromList [ (fromList ["table"] ,fromList [(["table"] , 3 )])]
         , (fromList ["object"],fromList [(["object"], 3 )])
         , (fromList ["glas"]  ,fromList [(["glas"]  , 2 )])
         , (fromList ["spoon"] ,fromList [(["spoon"] , 2 )])
-}
metrics_occ = occurrences <$> L.concat <$> metrics_terms

{- 
-- fromList [((["glas"],["object"]),6)
            ,((["glas"],["spoon"]),4)
            ,((["glas"],["table"]),6),((["object"],["spoon"]),6),((["object"],["table"]),9),((["spoon"],["table"]),6)]

-}
metrics_cooc = cooc <$> metrics_terms

metrics_cooc_mat = do
  m <- metrics_cooc
  let (ti,_) = createIndices m
  let mat_cooc = cooc2mat ti m
  pure ( ti
       , mat_cooc
       , incExcSpeGen_proba  mat_cooc
       , incExcSpeGen        mat_cooc
       )

metrics_incExcSpeGen = incExcSpeGen_sorted <$> metrics_cooc

