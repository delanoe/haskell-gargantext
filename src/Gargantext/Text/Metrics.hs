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

import Data.Ord (Down(..))
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map  as M

import Data.Text (Text)
import qualified Data.Text as T

import Data.Tuple.Extra (both)
--import GHC.Real (Ratio)
--import qualified Data.Text.Metrics as DTM
import Data.Array.Accelerate (toList, Matrix)
--import Math.KMeans (kmeans, euclidSq, elements)

import Gargantext.Prelude
import Gargantext.Text.Metrics.Count (occurrences, cooc)
import Gargantext.Text.Terms (TermType(MonoMulti), terms)
import Gargantext.Core (Lang(EN))
import Gargantext.Core.Types (Terms(..), Label)
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))
import Gargantext.Text.Metrics.Count (Grouped)
import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index

import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.Array.Accelerate as DAA
-- import Data.Array.Accelerate ((:.)(..), Z(..))

import GHC.Real (round)

--import Debug.Trace

data MapListSize   = MapListSize   Int
data InclusionSize = InclusionSize Int
data SampleBins    = SampleBins    Double
data Clusters      = Clusters      Int
data DefaultValue  = DefaultValue  Int

data FilterConfig = FilterConfig { fc_mapListSize   :: MapListSize
                                 , fc_inclusionSize :: InclusionSize
                                 , fc_sampleBins    :: SampleBins
                                 , fc_clusters      :: Clusters
                                 , fc_defaultValue  :: DefaultValue
                             }

filterCooc :: Ord t => FilterConfig -> Map (t, t) Int -> Map (t, t) Int
filterCooc fc cc = (filterCooc' fc) ts cc
  where
    ts     = map _scored_terms $ takeSome fc $ coocScored cc


filterCooc' :: Ord t => FilterConfig -> [t] -> Map (t, t) Int -> Map (t, t) Int
filterCooc' (FilterConfig _ _ _ _ (DefaultValue dv)) ts m = -- trace ("coocScored " <> show (length ts)) $
  foldl' (\m' k -> M.insert k (maybe dv identity $ M.lookup k m) m')
    M.empty selection
  where
    selection  = [(x,y) | x <- ts
                        , y <- ts
                       -- , x >= y
                        ]


-- | Map list creation
-- Kmeans split into (Clusters::Int) main clusters with Inclusion/Exclusion (relevance score)
-- Sample the main cluster ordered by specificity/genericity in (SampleBins::Double) parts
-- each parts is then ordered by Inclusion/Exclusion
-- take n scored terms in each parts where n * SampleBins = MapListSize.
takeSome :: Ord t => FilterConfig -> [Scored t] -> [Scored t]
takeSome (FilterConfig (MapListSize l) (InclusionSize l') (SampleBins s) (Clusters _) _) scores = L.take l
                    $ takeSample n m
                    $ L.take l' $ sortWith (Down . _scored_incExc) scores
                    -- $ splitKmeans k scores
  where
    -- TODO: benchmark with accelerate-example kmeans version
    --splitKmeans x xs = L.concat $ map elements
    --                 $ V.take (k-1)
    --                 $ kmeans (\i -> VU.fromList ([(_scored_incExc i :: Double)]))
    --                          euclidSq x xs
    n = round ((fromIntegral l)/s)
    m = round $ (fromIntegral $ length scores) / (s)
    takeSample n' m' xs = -- trace ("splitKmeans " <> show (length xs)) $
                        L.concat $ map (L.take n')
                                 $ map (sortWith (Down . _scored_incExc))
                                 -- TODO use kmeans s instead of splitEvery
                                 -- in order to split in s heteregenous parts
                                 -- without homogeneous order hypothesis
                                 $ splitEvery m'
                                 $ sortWith (Down . _scored_speGen) xs


data Scored t = Scored { _scored_terms  :: !t
                       , _scored_incExc :: !InclusionExclusion
                       , _scored_speGen :: !SpecificityGenericity
                     } deriving (Show)

-- TODO in the textflow we end up needing these indices, it might be better
-- to compute them earlier and pass them around.
coocScored :: Ord t => Map (t,t) Int -> [Scored t]
coocScored m = zipWith (\(_,t) (inc,spe) -> Scored t inc spe) (M.toList fi) scores
  where
    (ti,fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores = DAA.toList $ DAA.run $ DAA.zip (DAA.use is) (DAA.use ss)


















incExcSpeGen_sorted :: Ord t => Map (t,t) Int -> ([(t,Double)],[(t,Double)])
incExcSpeGen_sorted m = both ordonne (incExcSpeGen $ cooc2mat ti m)
  where
    (ti,fi) = createIndices m
    ordonne x = sortWith (Down . snd) $ zip (map snd $ M.toList fi) (toList x)



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

metrics_sentences_Test :: Bool
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
metrics_terms = mapM (terms (MonoMulti EN)) $ splitBy (Sentences 0) metrics_text

-- | Occurrences
{-
fromList [ (fromList ["table"] ,fromList [(["table"] , 3 )])]
         , (fromList ["object"],fromList [(["object"], 3 )])
         , (fromList ["glas"]  ,fromList [(["glas"]  , 2 )])
         , (fromList ["spoon"] ,fromList [(["spoon"] , 2 )])
-}
metrics_occ :: IO (Map Grouped (Map Terms Int))
metrics_occ = occurrences <$> L.concat <$> metrics_terms

{- 
-- fromList [((["glas"],["object"]),6)
            ,((["glas"],["spoon"]),4)
            ,((["glas"],["table"]),6),((["object"],["spoon"]),6),((["object"],["table"]),9),((["spoon"],["table"]),6)]

-}
metrics_cooc :: IO (Map (Label, Label) Int)
metrics_cooc = cooc <$> metrics_terms

metrics_cooc_mat :: IO (Map Label Index, Matrix Int, Matrix Double, (DAA.Vector InclusionExclusion, DAA.Vector SpecificityGenericity))
metrics_cooc_mat = do
  m <- metrics_cooc
  let (ti,_) = createIndices m
  let mat_cooc = cooc2mat ti m
  pure ( ti
       , mat_cooc
       , incExcSpeGen_proba  mat_cooc
       , incExcSpeGen        mat_cooc
       )

metrics_incExcSpeGen :: IO ([(Label, Double)], [(Label, Double)])
metrics_incExcSpeGen = incExcSpeGen_sorted <$> metrics_cooc

