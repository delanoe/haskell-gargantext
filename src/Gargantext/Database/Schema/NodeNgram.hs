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

{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}


-- TODO NodeNgrams
module Gargantext.Database.Schema.NodeNgram where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Database.Utils (mkCmd, Cmd, execPGSQuery)
import Gargantext.Core.Types.Main (ListTypeId)
import Gargantext.Database.Types.Node (NodeId, ListId)
import Gargantext.Database.Schema.Node (pgNodeId)
import Gargantext.Database.Schema.Ngrams (NgramsTypeId, pgNgramsTypeId)
import Gargantext.Prelude
import Gargantext.Database.Utils (formatPGSQuery)
import Opaleye
import qualified Database.PostgreSQL.Simple as DPS

-- | TODO : remove id
data NodeNgramPoly node_id ngrams_id parent_id ngrams_type list_type weight
   = NodeNgram { nng_node_id    :: node_id
               , nng_ngrams_id  :: ngrams_id
               , nng_parent_id  :: parent_id
              
               , nng_ngramsType :: ngrams_type
               , nng_listType   :: list_type
               , nng_weight     :: weight
               } deriving (Show)

type NodeNgramWrite =
     NodeNgramPoly
               (Column PGInt4  )
               (Column PGInt4  )
               (Maybe (Column PGInt4))
               
               (Column PGInt4  )
               (Column PGInt4  )
               (Column PGFloat8)

type NodeNgramRead =
     NodeNgramPoly
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGInt4  )
       
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGFloat8)

type NodeNgramReadNull =
     NodeNgramPoly
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGFloat8))

type NodeNgram =
     NodeNgramPoly NodeId Int (Maybe NgramsParentId) NgramsTypeId Int Double

newtype NgramsParentId = NgramsParentId Int
  deriving (Show, Eq, Num)

pgNgramsParentId :: NgramsParentId -> Column PGInt4
pgNgramsParentId (NgramsParentId n) = pgInt4 n

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
makeLenses ''NodeNgramPoly

nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams"
  ( pNodeNgram NodeNgram
    { nng_node_id    = required "node_id"
    , nng_ngrams_id  = required "ngrams_id"
    , nng_parent_id  = optional "parent_id"
    , nng_ngramsType = required "ngrams_type"
    , nng_listType   = required "list_type"
    , nng_weight     = required "weight"
    }
  )

queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable

--{-
insertNodeNgrams :: [NodeNgram] -> Cmd err Int
insertNodeNgrams = insertNodeNgramW
                 . map (\(NodeNgram n g p ngt lt w) ->
                          NodeNgram (pgNodeId n)
                                    (pgInt4 g)
                                    (pgNgramsParentId <$> p)
                                    (pgNgramsTypeId ngt)
                                    (pgInt4 lt)
                                    (pgDouble w)
                        )
insertNodeNgramW :: [NodeNgramWrite] -> Cmd err Int
insertNodeNgramW nns =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = (Insert { iTable = nodeNgramTable
                              , iRows  = nns
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
                              })
--}
type NgramsText = Text

updateNodeNgrams' :: ListId -> [(NgramsTypeId, NgramsText, ListTypeId)] -> Cmd err ()
updateNodeNgrams' _      []    = pure ()
updateNodeNgrams' listId input = void $ execPGSQuery updateQuery (DPS.Only $ Values fields input')
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","text","int4"]
    input' = map (\(nt,t,lt) -> (listId, nt, t, lt)) input

updateNodeNgrams'_debug :: ListId -> [(NgramsTypeId, NgramsText, ListTypeId)] -> Cmd err ByteString
updateNodeNgrams'_debug listId input = formatPGSQuery updateQuery (DPS.Only $ Values fields input')
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","text","int4"]
    input' = map (\(nt,t,lt) -> (listId, nt, t, lt)) input

updateQuery :: DPS.Query
updateQuery = [sql|
WITH new(node_id,ngrams_type,terms,typeList) as (?)

INSERT into nodes_ngrams (node_id,ngrams_id,ngrams_type,list_type,weight)

SELECT node_id,ngrams.id,ngrams_type,typeList,1 FROM new
JOIN ngrams ON ngrams.terms = new.terms
ON CONFLICT (node_id, ngrams_id, ngrams_type) DO
-- DO NOTHING

UPDATE SET list_type = excluded.list_type
;
               |]
