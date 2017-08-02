{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.NodeNgram where

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


nodeNgramTable :: O.Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = O.Table "nodes_ngrams" (pNodeNgram NodeNgram { nodeNgram_NodeNgramId       = O.optional "id"
                                                               , nodeNgram_NodeNgramNodeId   = O.required "node_id"
                                                               , nodeNgram_NodeNgramNgramId  = O.required "ngram_id"
                                                               , nodeNgram_NodeNgramWeight   = O.optional "weight"
                                                               }
                                         )


queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = O.queryTable nodeNgramTable


-- | not optimized (get all ngrams without filters)
nodeNgrams :: IO [NodeNgram]
nodeNgrams = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn queryNodeNgramTable

