{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.Database.NodeNgramNgram where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye

import Gargantext.Database.Private (infoGargandb)

data NodeNgramNgramPoly node_id ngram1_id ngram2_id weight
                   = NodeNgramNgram { nodeNgramNgram_NodeNgramNgram_NodeId   :: node_id
                                    , nodeNgramNgram_NodeNgramNgram_Ngram1Id :: ngram1_id
                                    , nodeNgramNgram_NodeNgramNgram_Ngram2Id :: ngram2_id
                                    , nodeNgramNgram_NodeNgramNgram_Weight   :: weight
                                    } deriving (Show)


type NodeNgramNgramWrite = NodeNgramNgramPoly (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Maybe (Column PGFloat8))
type NodeNgramNgramRead  = NodeNgramNgramPoly        (Column PGInt4)  (Column PGInt4) (Column PGInt4) (Column PGFloat8)


type NodeNgramNgram = NodeNgramNgramPoly (Maybe Int) Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNgramNgram" ''NodeNgramNgramPoly)
$(makeLensesWith abbreviatedFields         ''NodeNgramNgramPoly)


nodeNgramNgramTable :: Table NodeNgramNgramWrite NodeNgramNgramRead
nodeNgramNgramTable  = Table "nodes_ngrams_ngrams" ( pNodeNgramNgram NodeNgramNgram
                                                     { nodeNgramNgram_NodeNgramNgram_NodeId   = optional "node_id"
                                                     , nodeNgramNgram_NodeNgramNgram_Ngram1Id = required "ngram1_id"
                                                     , nodeNgramNgram_NodeNgramNgram_Ngram2Id = required "ngram2_id"
                                                     , nodeNgramNgram_NodeNgramNgram_Weight   = optional "weight"
                                                     }
                                                   )


queryNodeNgramNgramTable :: Query NodeNgramNgramRead
queryNodeNgramNgramTable = queryTable nodeNgramNgramTable


-- | not optimized (get all ngrams without filters)
nodeNgramNgrams :: IO [NodeNgramNgram]
nodeNgramNgrams = do
    conn <- PGS.connect infoGargandb
    runQuery conn queryNodeNgramNgramTable

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
