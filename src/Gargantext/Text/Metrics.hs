{-|
Module      : Gargantext.Text.Metrics
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly reexport functions in @Data.Text.Metrics@
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List (concat)

--import GHC.Real (Ratio)
--import qualified Data.Text.Metrics as DTM

import Gargantext.Prelude

import Gargantext.Text.Metrics.Count (occurrences, cooc)
import Gargantext.Text.Terms (TermType(Multi), terms)
import Gargantext.Core (Lang(EN))
import Gargantext.Core.Types (Terms(..))
import Gargantext.Text.Context (splitBy, SplitContext(Sentences))

--noApax :: Ord a => Map a Occ -> Map a Occ
--noApax m = M.filter (>1) m


metrics_text :: Text
metrics_text = T.intercalate " " ["A table is an object."
                        ,"A glas is an object too."
                        ,"Using a glas to dring is a function."
                        ,"Using a spoon to eat is a function."
                        ,"The spoon is an object to eat."
                        ]

metrics_sentences' :: [Text]
metrics_sentences' = splitBy (Sentences 0) metrics_text

-- | Sentences 
metrics_sentences :: [Text]
metrics_sentences = ["A table is an object."
                    ,"A glas is an object too."
                    ,"The glas and the spoon are on the table."
                    ,"The spoon is an object to eat."
                    ,"The spoon is on the table and the plate and the glas."]


metrics_sentences_Test = metrics_sentences == metrics_sentences'

-- | Terms reordered to visually check occurrences
metrics_terms :: [[Text]]
metrics_terms = undefined

metrics_terms' :: IO [[Terms]]
metrics_terms' = mapM (terms Multi EN) $ splitBy (Sentences 0) metrics_text

--metrics_terms_Test =  metrics_terms == ((map _terms_label) <$> metrics_terms')

-- | Occurrences
{-
fromList [ (fromList ["table"] ,fromList [(["table"] , 3 )])]
         , (fromList ["object"],fromList [(["object"], 3 )])
         , (fromList ["glas"]  ,fromList [(["glas"]  , 2 )])
         , (fromList ["spoon"] ,fromList [(["spoon"] , 2 )])
-}
metrics_occ = occurrences <$> concat <$> (mapM (terms Multi EN) $ splitBy (Sentences 0) metrics_text)

{- 
-- fromList [((["glas"],["object"]),6)
            ,((["glas"],["spoon"]),4)
            ,((["glas"],["table"]),6),((["object"],["spoon"]),6),((["object"],["table"]),9),((["spoon"],["table"]),6)]

-}
metrics_cooc = cooc <$> (mapM (terms Multi EN) $ splitBy (Sentences 0) metrics_text)

metrics_cooc' = (mapM (terms Multi EN) $ splitBy (Sentences 0) "The table object. The table object.")






