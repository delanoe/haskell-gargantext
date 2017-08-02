{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.NodeNodeNgram where

import Prelude
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS

import qualified Opaleye as O
import Opaleye (Column, PGBool, PGInt4, PGText, PGTimestamptz, PGFloat8
               , Table(Table), Query
               , QueryRunnerColumnDefault, queryRunnerColumnDefault 
               , fieldQueryRunnerColumn 
               , (.==), (.>)
               )

import Data.Gargantext.Database.Private (infoGargandb)
import Data.Gargantext.Database.Instances

data NodeNodeNgramPoly node1_id node2_id ngram_id score
                   = NodeNodeNgram { nodeNodeNgram_node1_id :: node1_id
                                   , nodeNodeNgram_node2_id :: node2_id
                                   , nodeNodeNgram_ngram_id :: ngram_id
                                   , nodeNodeNgram_score   :: score
                                   } deriving (Show)


type NodeNodeNgramWrite = NodeNodeNgramPoly (Column PGInt4) (Column PGInt4) (Column PGInt4) (Maybe (Column PGFloat8))
type NodeNodeNgramRead  = NodeNodeNgramPoly (Column PGInt4) (Column PGInt4) (Column PGInt4)        (Column PGFloat8)


type NodeNodeNgram = NodeNodeNgramPoly Int Int Int (Maybe Double)

$(makeAdaptorAndInstance "pNodeNodeNgram" ''NodeNodeNgramPoly)
$(makeLensesWith abbreviatedFields        ''NodeNodeNgramPoly)


nodeNodeNgramTable :: O.Table NodeNodeNgramWrite NodeNodeNgramRead
nodeNodeNgramTable  = O.Table "nodes_nodes_ngrams" ( pNodeNodeNgram NodeNodeNgram 
                                                         { nodeNodeNgram_node1_id = O.required "node1_id"
                                                         , nodeNodeNgram_node2_id = O.required "node2_id"
                                                         , nodeNodeNgram_ngram_id = O.required "ngram_id"
                                                         , nodeNodeNgram_score   = O.optional "score"
                                                         }
                                                     )


queryNodeNodeNgramTable :: Query NodeNodeNgramRead
queryNodeNodeNgramTable = O.queryTable nodeNodeNgramTable


-- | not optimized (get all ngrams without filters)
nodeNodeNgrams :: IO [NodeNodeNgram]
nodeNodeNgrams = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn queryNodeNodeNgramTable

