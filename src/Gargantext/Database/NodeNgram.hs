{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Gargantext.Database.NodeNgram where

import Prelude
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)

import Opaleye

data NodeNgramPoly id node_id ngram_id weight
                   = NodeNgram { nodeNgram_NodeNgramId      :: id
                               , nodeNgram_NodeNgramNodeId  :: node_id
                               , nodeNgram_NodeNgramNgramId :: ngram_id
                               , nodeNgram_NodeNgramWeight  :: weight
                               } deriving (Show)

type NodeNgramWrite = NodeNgramPoly (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGFloat8)

type NodeNgramRead  = NodeNgramPoly (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGFloat8)

type NodeNgram = NodeNgramPoly Int Int Int Double

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
$(makeLensesWith abbreviatedFields    ''NodeNgramPoly)


nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams" ( pNodeNgram NodeNgram 
                                           { nodeNgram_NodeNgramId = required "id"
                                           , nodeNgram_NodeNgramNodeId   = required "node_id"
                                           , nodeNgram_NodeNgramNgramId  = required "ngram_id"
                                           , nodeNgram_NodeNgramWeight   = required "weight"
                                           }
                                       )

queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable

