{-|
Module      : Gargantext.Database.NodeNgram
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

