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

import Gargantext.Prelude
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Gargantext.Database.Node (mkCmd, Cmd(..))
import Opaleye

data NodeNgramPoly id node_id ngram_id weight ngrams_type
                   = NodeNgram { nodeNgram_NodeNgramId      :: id
                               , nodeNgram_NodeNgramNodeId  :: node_id
                               , nodeNgram_NodeNgramNgramId :: ngram_id
                               , nodeNgram_NodeNgramWeight  :: weight
                               , nodeNgram_NodeNgramType    :: ngrams_type
                               } deriving (Show)

type NodeNgramWrite = NodeNgramPoly (Maybe (Column PGInt4  ))
                                    (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGFloat8)
                                    (Column PGInt4  )

type NodeNgramRead  = NodeNgramPoly (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGInt4  )
                                    (Column PGFloat8)
                                    (Column PGInt4  )

type NodeNgram = NodeNgramPoly (Maybe Int) Int Int Double Int

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
$(makeLensesWith abbreviatedFields    ''NodeNgramPoly)


nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams" ( pNodeNgram NodeNgram 
                                           { nodeNgram_NodeNgramId = optional "id"
                                           , nodeNgram_NodeNgramNodeId  = required "node_id"
                                           , nodeNgram_NodeNgramNgramId = required "ngram_id"
                                           , nodeNgram_NodeNgramWeight  = required "weight"
                                           , nodeNgram_NodeNgramType    = required "ngrams_type"
                                           }
                                       )

queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable

insertNodeNgrams :: [NodeNgram] -> Cmd Int
insertNodeNgrams nns = insertNodeNgramW $ map (\(NodeNgram i n g w t) -> NodeNgram Nothing (pgInt4 n) (pgInt4 g) (pgDouble w) (pgInt4 t) ) nns

insertNodeNgramW :: [NodeNgramWrite] -> Cmd Int
insertNodeNgramW nns = mkCmd $ \c -> fromIntegral <$> runInsertMany c nodeNgramTable nns


