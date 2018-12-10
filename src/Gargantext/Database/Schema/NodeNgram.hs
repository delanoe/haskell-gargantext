{-|
Module      : Gargantext.Database.Schema.NodeNgrams
Description : NodeNgram for Ngram indexation or Lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgram: relation between a Node and a Ngrams

if Node is a Document then it is indexing
if Node is a List     then it is listing (either Stop, Candidate or Map)

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}


-- TODO NodeNgrams
module Gargantext.Database.Schema.NodeNgram where

import Data.Text (Text)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core.Types.Main (ListId, ListTypeId)
import Gargantext.Database.Utils (mkCmd, Cmd(..))
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Connection, query, Only(..))

-- | TODO : remove id
data NodeNgramPoly id node_id ngram_id weight ngrams_type
   = NodeNgram { nodeNgram_NodeNgramId      :: id
               , nodeNgram_NodeNgramNodeId  :: node_id
               , nodeNgram_NodeNgramNgramId :: ngram_id
               , nodeNgram_NodeNgramWeight  :: weight
               , nodeNgram_NodeNgramType    :: ngrams_type
               } deriving (Show)

type NodeNgramWrite =
     NodeNgramPoly
        (Maybe (Column PGInt4  ))
               (Column PGInt4  )
               (Column PGInt4  )
               (Column PGFloat8)
               (Column PGInt4  )

type NodeNgramRead =
     NodeNgramPoly
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGFloat8)
       (Column PGInt4  )

type NodeNgramReadNull =
     NodeNgramPoly
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGFloat8))
       (Column (Nullable PGInt4  ))

type NodeNgram =
     NodeNgramPoly (Maybe Int) Int Int Double Int

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
$(makeLensesWith abbreviatedFields    ''NodeNgramPoly)


nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams"
  ( pNodeNgram NodeNgram
    { nodeNgram_NodeNgramId      = optional "id"
    , nodeNgram_NodeNgramNodeId  = required "node_id"
    , nodeNgram_NodeNgramNgramId = required "ngram_id"
    , nodeNgram_NodeNgramWeight  = required "weight"
    , nodeNgram_NodeNgramType    = required "ngrams_type"
    }
  )

queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable

insertNodeNgrams :: [NodeNgram] -> Cmd Int
insertNodeNgrams = insertNodeNgramW
                 . map (\(NodeNgram _ n g w t) ->
                          NodeNgram Nothing (pgInt4 n)   (pgInt4 g)
                                            (pgDouble w) (pgInt4 t)
                        )

insertNodeNgramW :: [NodeNgramWrite] -> Cmd Int
insertNodeNgramW nns =
  mkCmd $ \c -> fromIntegral
       <$> runInsertManyOnConflictDoNothing c nodeNgramTable nns

type NgramsText = Text

updateNodeNgrams :: PGS.Connection -> [(ListId, NgramsText, ListTypeId)] -> IO [PGS.Only Int]
updateNodeNgrams c input = PGS.query c updateQuery (PGS.Only $ Values fields $ input)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","text","int4"]
    updateQuery = [sql| UPDATE nodes_ngrams as old SET
                 ngrams_type = new.typeList
                 from (?) as new(node_id,terms,typeList)
                 JOIN ngrams ON ngrams.terms = new.terms
                 WHERE old.node_id = new.node_id
                 AND   old.ngram_id = ngrams.id;
                 -- RETURNING new.ngram_id
                 |]

