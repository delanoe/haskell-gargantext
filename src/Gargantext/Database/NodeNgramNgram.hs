{-|
Module      : Gargantext.Database.NodeNgramNgram
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.NodeNgramNgram where

import Gargantext.Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye

data NodeNgramNgramPoly node_id ngram1_id ngram2_id weight =
  NodeNgramNgram { nng_NodeId   :: node_id
                 , nng_Ngram1Id :: ngram1_id
                 , nng_Ngram2Id :: ngram2_id
                 , nng_Weight   :: weight
                 } deriving (Show)


type NodeNgramNgramWrite =
  NodeNgramNgramPoly (Maybe (Column PGInt4  ))
                     (Column PGInt4          )
                     (Column PGInt4          )
                     (Maybe (Column PGFloat8))

type NodeNgramNgramRead  =
  NodeNgramNgramPoly (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGFloat8)

type NodeNgramNgram =
  NodeNgramNgramPoly (Maybe Int   )
                            Int
                            Int
                     (Maybe Double)

$(makeAdaptorAndInstance "pNodeNgramNgram"
                         ''NodeNgramNgramPoly)
$(makeLensesWith abbreviatedFields
                         ''NodeNgramNgramPoly)


nodeNgramNgramTable :: Table NodeNgramNgramWrite NodeNgramNgramRead
nodeNgramNgramTable  =
  Table "nodes_ngrams_ngrams"
       ( pNodeNgramNgram NodeNgramNgram
                       { nng_NodeId   = optional "node_id"
                       , nng_Ngram1Id = required "ngram1_id"
                       , nng_Ngram2Id = required "ngram2_id"
                       , nng_Weight   = optional "weight"
                       }
       )

queryNodeNgramNgramTable :: Query NodeNgramNgramRead
queryNodeNgramNgramTable = queryTable nodeNgramNgramTable

-- | not optimized (get all ngrams without filters)
nodeNgramNgrams :: PGS.Connection -> IO [NodeNgramNgram]
nodeNgramNgrams conn = runQuery conn queryNodeNgramNgramTable

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


