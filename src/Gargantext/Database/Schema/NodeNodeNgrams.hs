{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Gargantext.Database.Schema.NodeNodeNgrams
  where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Gargantext.Database.Utils (Cmd, runOpaQuery)

import Opaleye


data NodeNodeNgramsPoly node1_id node2_id ngram_id score
                   = NodeNodeNgrams { nnng_node1_id :: node1_id
                                    , nnng_node2_id :: node2_id
                                    , nnng_ngrams_id :: ngram_id
                                    , nnng_score   :: score
                                    } deriving (Show)


type NodeNodeNgramsWrite = NodeNodeNgramsPoly (Column PGInt4          )
                                            (Column PGInt4          )
                                            (Column PGInt4          )
                                            (Maybe (Column PGFloat8))

type NodeNodeNgramsRead  = NodeNodeNgramsPoly (Column PGInt4  )
                                            (Column PGInt4  )
                                            (Column PGInt4  )
                                            (Column PGFloat8)

type NodeNodeNgramsReadNull  = NodeNodeNgramsPoly (Column (Nullable PGInt4  ))
                                                (Column (Nullable PGInt4  ))
                                                (Column (Nullable PGInt4  ))
                                                (Column (Nullable PGFloat8))

type NodeNodeNgrams = NodeNodeNgramsPoly Int
                                       Int
                                       Int 
                                (Maybe Double)


$(makeAdaptorAndInstance "pNodeNodeNgrams" ''NodeNodeNgramsPoly)
$(makeLensesWith abbreviatedFields        ''NodeNodeNgramsPoly)

nodeNodeNgramsTable :: Table NodeNodeNgramsWrite NodeNodeNgramsRead
nodeNodeNgramsTable  = Table "nodes_nodes_ngrams" 
                          ( pNodeNodeNgrams NodeNodeNgrams
                               { nnng_node1_id  = required "node1_id"
                               , nnng_node2_id  = required "node2_id"
                               , nnng_ngrams_id = required "ngram_id"
                               , nnng_score     = optional "score"
                               }
                          )


queryNodeNodeNgramsTable :: Query NodeNodeNgramsRead
queryNodeNodeNgramsTable = queryTable nodeNodeNgramsTable

-- | not optimized (get all ngrams without filters)
nodeNodeNgrams :: Cmd err [NodeNodeNgrams]
nodeNodeNgrams = runOpaQuery queryNodeNodeNgramsTable

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
