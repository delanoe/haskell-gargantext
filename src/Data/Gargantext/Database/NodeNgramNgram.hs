{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.NodeNgramNgram where

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


nodeNgramNgramTable :: O.Table NodeNgramNgramWrite NodeNgramNgramRead
nodeNgramNgramTable  = O.Table "nodes_ngrams_ngrams" ( pNodeNgramNgram NodeNgramNgram 
                                                         { nodeNgramNgram_NodeNgramNgram_NodeId   = O.optional "node_id"
                                                         , nodeNgramNgram_NodeNgramNgram_Ngram1Id = O.required "ngram1_id"
                                                         , nodeNgramNgram_NodeNgramNgram_Ngram2Id = O.required "ngram2_id"
                                                         , nodeNgramNgram_NodeNgramNgram_Weight   = O.optional "weight"
                                                         }
                                                     )


queryNodeNgramNgramTable :: Query NodeNgramNgramRead
queryNodeNgramNgramTable = O.queryTable nodeNgramNgramTable


-- | not optimized (get all ngrams without filters)
nodeNgramNgrams :: IO [NodeNgramNgram]
nodeNgramNgrams = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn queryNodeNgramNgramTable

