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
import Data.Tuple.Extra (both)
--import GHC.Real (Ratio)
--import qualified Data.Text.Metrics as DTM
import Data.Array.Accelerate (toList)


import Gargantext.Prelude

import Gargantext.Text.Metrics.Count (occurrences, cooc)
import Gargantext.Text.Terms (TermType(MonoMulti), terms)
import Gargantext.Core (Lang(EN))
import Gargantext.Core.Types (Terms(..))
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index


-- ord relevance: top n plus inclus
-- échantillonnage de généricity
-- 
--filterCooc :: Ord t => Map (t, t) Int -> Map (t, t) Int
--filterCooc m = 
---- filterCooc m = foldl (\k -> maybe (panic "no key") identity $ M.lookup k m) M.empty selection
----(ti, fi)  = createIndices m
-- . fromIndex fi $ filterMat $ cooc2mat ti m


import Data.Array.Accelerate (Matrix)

filterMat :: Matrix Int -> [(Index, Index)]
filterMat m = S.toList $ S.take n $ S.fromList $ (L.take nIe incExc') <> (L.take nSg speGen')
  where
    (incExc', speGen') = both ( map fst . L.sortOn snd . M.toList . mat2map) (conditional' m)
    n = nIe + nSg
    nIe = 30
    nSg = 70



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

