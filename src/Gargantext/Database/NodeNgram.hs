{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Gargantext.Database.NodeNgram where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye


data NodeNgramPoly id node_id ngram_id weight
                   = NodeNgram { nodeNgram_NodeNgramId      :: id
                               , nodeNgram_NodeNgramNodeId  :: node_id
                               , nodeNgram_NodeNgramNgramId :: ngram_id
                               , nodeNgram_NodeNgramWeight  :: weight
                               } deriving (Show)

type NodeNgramWrite = NodeNgramPoly (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Maybe (Column PGFloat8))
type NodeNgramRead  = NodeNgramPoly        (Column PGInt4)  (Column PGInt4) (Column PGInt4) ((Column PGFloat8))


type NodeNgram = NodeNgramPoly (Maybe Int) Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
$(makeLensesWith abbreviatedFields    ''NodeNgramPoly)


nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams" (pNodeNgram NodeNgram { nodeNgram_NodeNgramId       = optional "id"
                                                             , nodeNgram_NodeNgramNodeId   = required "node_id"
                                                             , nodeNgram_NodeNgramNgramId  = required "ngram_id"
                                                             , nodeNgram_NodeNgramWeight   = optional "weight"
                                                             }
                                       )


queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable


instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


-- | not optimized (get all ngrams without filters)
nodeNgrams :: PGS.Connection -> IO [NodeNgram]
nodeNgrams conn = runQuery conn queryNodeNgramTable
