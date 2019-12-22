{-|
Module      : Gargantext.Database.Schema.NodeNgramsNgrams
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgramsNgrams table is used to group Ngrams
- NodeId :: List Id
- NgramId_1, NgramId_2 where all NgramId_2 will be added to NgramId_1
- weight: probability of the relation (TODO, fixed to 1 for simple stemming)

Next Step benchmark:
- recursive queries of postgres
- group with: https://en.wikipedia.org/wiki/Nested_set_model

-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Database.Schema.NodeNgramsNgrams
  where

import Control.Lens (view)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Gargantext.Database.Utils (Cmd, runOpaQuery, connection)
import Gargantext.Database.Types.Node (ListId)
import Gargantext.Database.Schema.Node (pgNodeId)
import Gargantext.Prelude
import Opaleye

data NodeNgramsNgramsPoly node_id ngram1_id ngram2_id weight =
  NodeNgramsNgrams { _nng_NodeId   :: node_id
                   , _nng_Ngram1Id :: ngram1_id
                   , _nng_Ngram2Id :: ngram2_id
                   , _nng_Weight   :: weight
                   } deriving (Show)

type NodeNgramsNgramsWrite =
  NodeNgramsNgramsPoly (Column PGInt4          )
                     (Column PGInt4          )
                     (Column PGInt4          )
                     (Maybe (Column PGFloat8))

type NodeNgramsNgramsRead  =
  NodeNgramsNgramsPoly (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGFloat8)

type NodeNgramsNgrams =
  NodeNgramsNgramsPoly ListId
                     Int
                     Int
                    (Maybe Double)

$(makeAdaptorAndInstance "pNodeNgramsNgrams"
                         ''NodeNgramsNgramsPoly)
$(makeLensesWith abbreviatedFields
                         ''NodeNgramsNgramsPoly)


nodeNgramsNgramsTable :: Table NodeNgramsNgramsWrite NodeNgramsNgramsRead
nodeNgramsNgramsTable  =
  Table "nodes_ngrams_ngrams"
       ( pNodeNgramsNgrams NodeNgramsNgrams
                       { _nng_NodeId   = required "node_id"
                       , _nng_Ngram1Id = required "ngram1_id"
                       , _nng_Ngram2Id = required "ngram2_id"
                       , _nng_Weight   = optional "weight"
                       }
       )

queryNodeNgramsNgramsTable :: Query NodeNgramsNgramsRead
queryNodeNgramsNgramsTable = queryTable nodeNgramsNgramsTable

-- | Select NodeNgramsNgrams
-- TODO not optimized (get all ngrams without filters)
nodeNgramsNgrams :: Cmd err [NodeNgramsNgrams]
nodeNgramsNgrams = runOpaQuery queryNodeNgramsNgramsTable

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


-- TODO: Add option on conflict
insertNodeNgramsNgramsNew :: [NodeNgramsNgrams] -> Cmd err Int64
insertNodeNgramsNgramsNew = insertNodeNgramsNgramsW
                 . map (\(NodeNgramsNgrams n ng1 ng2 maybeWeight) ->
                          NodeNgramsNgrams (pgNodeId n  )
                                           (pgInt4 ng1)
                                           (pgInt4 ng2)
                                           (pgDouble <$> maybeWeight)
                        )

insertNodeNgramsNgramsW :: [NodeNgramsNgramsWrite] -> Cmd err Int64
insertNodeNgramsNgramsW ns = do
  c <- view connection
  liftIO $ runInsertMany c nodeNgramsNgramsTable ns

