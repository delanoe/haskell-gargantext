{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Gargantext.Database.NodeNodeNgram where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye


data NodeNodeNgramPoly node1_id node2_id ngram_id score
                   = NodeNodeNgram { nodeNodeNgram_node1_id :: node1_id
                                   , nodeNodeNgram_node2_id :: node2_id
                                   , nodeNodeNgram_ngram_id :: ngram_id
                                   , nodeNodeNgram_score   :: score
                                   } deriving (Show)


type NodeNodeNgramWrite = NodeNodeNgramPoly (Column PGInt4) (Column PGInt4) (Column PGInt4) (Maybe (Column PGFloat8))
type NodeNodeNgramRead  = NodeNodeNgramPoly (Column PGInt4) (Column PGInt4) (Column PGInt4)        (Column PGFloat8)

type NodeNodeNgramReadNull  = NodeNodeNgramPoly (Column(Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGFloat8))

type NodeNodeNgram = NodeNodeNgramPoly Int Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNodeNgram" ''NodeNodeNgramPoly)
$(makeLensesWith abbreviatedFields        ''NodeNodeNgramPoly)


nodeNodeNgramTable :: Table NodeNodeNgramWrite NodeNodeNgramRead
nodeNodeNgramTable  = Table "nodes_nodes_ngrams" ( pNodeNodeNgram NodeNodeNgram
                                                   { nodeNodeNgram_node1_id = required "node1_id"
                                                   , nodeNodeNgram_node2_id = required "node2_id"
                                                   , nodeNodeNgram_ngram_id = required "ngram_id"
                                                   , nodeNodeNgram_score    = optional "score"
                                                   }
                                                 )


queryNodeNodeNgramTable :: Query NodeNodeNgramRead
queryNodeNodeNgramTable = queryTable nodeNodeNgramTable


-- | not optimized (get all ngrams without filters)
nodeNodeNgrams :: PGS.Connection -> IO [NodeNodeNgram]
nodeNodeNgrams conn = runQuery conn queryNodeNodeNgramTable


instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
