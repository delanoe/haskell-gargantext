{-|
Module      : Gargantext.Database.NodeNgramsNgrams
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
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Database.NodeNgramsNgrams
  where

import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Node (mkCmd, Cmd(..))
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as DPS

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
  NodeNgramsNgramsPoly Int
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
nodeNgramsNgrams :: DPS.Connection -> IO [NodeNgramsNgrams]
nodeNgramsNgrams conn = runQuery conn queryNodeNgramsNgramsTable

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


-- TODO: Add option on conflict
insertNodeNgramsNgramsNew :: [NodeNgramsNgrams] -> Cmd Int
insertNodeNgramsNgramsNew = insertNodeNgramsNgramsW
                 . map (\(NodeNgramsNgrams n ng1 ng2 maybeWeight) ->
                          NodeNgramsNgrams (pgInt4 n  )
                                           (pgInt4 ng1)
                                           (pgInt4 ng2)
                                           (pgDouble <$> maybeWeight)
                        )

insertNodeNgramsNgramsW :: [NodeNgramsNgramsWrite] -> Cmd Int
insertNodeNgramsNgramsW ns =
  mkCmd $ \c -> fromIntegral
       <$> runInsertMany c nodeNgramsNgramsTable ns

------------------------------------------------------------------------
data Action   = Del | Add

ngramsGroup :: Action -> [NodeNgramsNgrams] -> Cmd [Int]
ngramsGroup a ngs = mkCmd $ \c -> ngramsGroup' c a ngs

-- TODO: remove this function (use Reader Monad only)
ngramsGroup' :: DPS.Connection -> Action -> [NodeNgramsNgrams] -> IO [Int]
ngramsGroup' c action ngs = runNodeNgramsNgrams c q ngs
  where
    q = case action of
          Del -> queryDelNodeNgramsNgrams
          Add -> queryInsertNodeNgramsNgrams


runNodeNgramsNgrams :: DPS.Connection -> DPS.Query -> [NodeNgramsNgrams] -> IO [Int]
runNodeNgramsNgrams c q ngs = map (\(DPS.Only a) -> a) <$> DPS.query c q (DPS.Only $ Values fields ngs' )
  where
    ngs'   = map (\(NodeNgramsNgrams n ng1 ng2 w) -> (n,ng1,ng2,maybe 0 identity w)) ngs
    fields = map (\t -> QualifiedIdentifier Nothing t)
                 ["int4","int4","int4","double"]

--------------------------------------------------------------------
-- TODO: on conflict update weight
queryInsertNodeNgramsNgrams :: DPS.Query
queryInsertNodeNgramsNgrams = [sql|
    WITH input_rows(nId,ng1,ng2,w) AS (?)
    , ins AS (
       INSERT INTO nodes_ngrams_ngrams (node_id,ngram1_id,ngram2_id,weight)
       SELECT * FROM input_rows
       ON CONFLICT (node_id,ngram1_id,ngram2_id) DO NOTHING -- unique index created here
       )
           |]

queryDelNodeNgramsNgrams :: DPS.Query
queryDelNodeNgramsNgrams = [sql|
    WITH input(nId,ng1,ng2,w) AS (?)
    , DELETE FROM nodes_ngrams_ngrams
    WHERE   node_id = input.nId
      AND ngram1_id = input.ng1
      AND ngram2_id = input.ng2
       ;)
           |]
