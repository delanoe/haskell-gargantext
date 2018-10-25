{-|
Module      : Gargantext.Database.NodeNgramNgram
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgramNgram table is used to group Ngrams
- NodeId :: List Id
- NgramId_1, NgramId_2 where all NgramId_2 will be added to NgramId_1
- weight: probability of the relation (TODO, fixed to 1 for simple stemming)

Next Step benchmark:
- recursive queries of postgres
- group with: https://en.wikipedia.org/wiki/Nested_set_model

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.NodeNgramNgram
  where

import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Gargantext.Database.Node (mkCmd, Cmd(..))
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS

data NodeNgramNgramPoly node_id ngram1_id ngram2_id weight =
  NodeNgramNgram { nng_NodeId   :: node_id
                 , nng_Ngram1Id :: ngram1_id
                 , nng_Ngram2Id :: ngram2_id
                 , nng_Weight   :: weight
                 } deriving (Show)


type NodeNgramNgramWrite =
  NodeNgramNgramPoly (Column PGInt4          )
                     (Column PGInt4          )
                     (Column PGInt4          )
                     (Maybe (Column PGFloat8))

type NodeNgramNgramRead  =
  NodeNgramNgramPoly (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGInt4  )
                     (Column PGFloat8)

type NodeNgramNgram =
  NodeNgramNgramPoly Int
                     Int
                     Int
                    (Maybe Double)

$(makeAdaptorAndInstance "pNodeNgramNgram"
                         ''NodeNgramNgramPoly)
$(makeLensesWith abbreviatedFields
                         ''NodeNgramNgramPoly)


nodeNgramNgramTable :: Table NodeNgramNgramWrite NodeNgramNgramRead
nodeNgramNgramTable  =
  Table "nodes_ngrams_ngrams"
       ( pNodeNgramNgram NodeNgramNgram
                       { nng_NodeId   = required "node_id"
                       , nng_Ngram1Id = required "ngram1_id"
                       , nng_Ngram2Id = required "ngram2_id"
                       , nng_Weight   = optional "weight"
                       }
       )

queryNodeNgramNgramTable :: Query NodeNgramNgramRead
queryNodeNgramNgramTable = queryTable nodeNgramNgramTable

-- | Select NodeNgramNgram
-- TODO not optimized (get all ngrams without filters)
nodeNgramNgram :: PGS.Connection -> IO [NodeNgramNgram]
nodeNgramNgram conn = runQuery conn queryNodeNgramNgramTable

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


insertNodeNgramNgram :: [NodeNgramNgram] -> Cmd Int
insertNodeNgramNgram = insertNodeNgramNgramW
                 . map (\(NodeNgramNgram n ng1 ng2 maybeWeight) ->
                          NodeNgramNgram (pgInt4 n)
                                         (pgInt4 ng1)
                                         (pgInt4 ng2)
                                         (pgDouble <$> maybeWeight)
                        )


insertNodeNgramNgramW :: [NodeNgramNgramWrite] -> Cmd Int
insertNodeNgramNgramW ns =
  mkCmd $ \c -> fromIntegral
       <$> runInsertMany c nodeNgramNgramTable ns


