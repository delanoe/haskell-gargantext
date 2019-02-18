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
import Debug.Trace (trace)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Database.Utils (mkCmd, Cmd, execPGSQuery)
import Gargantext.Core.Types.Main (ListTypeId)
import Gargantext.Database.Types.Node (NodeId, ListId, NodeType(..))
import Gargantext.Database.Config (nodeTypeId, userMaster)
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

data Action   = Del | Add
type NgramsParent = Text
type NgramsChild  = Text

ngramsGroup :: Action -> ListId -> [(NgramsTypeId, NgramsParent, NgramsChild)] -> Cmd err ()
ngramsGroup _ _ [] = pure ()
ngramsGroup a lid input = void $ trace (show input) $ execPGSQuery (ngramsGroupQuery a) (DPS.Only $ Values fields input')
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","text","int4","text","text"]
    input' = map (\(ntpid,p,c) -> (lid, nodeTypeId NodeList, userMaster, ntpid, p,c)) input


ngramsGroupQuery :: Action -> DPS.Query
ngramsGroupQuery a = case a of
  Add -> [sql|
WITH input(lid,listTypeId,masterUsername,ntype,parent_terms,child_terms)
  AS (?),
  -- (VALUES (15::"int4",5::"int4",'gargantua'::"text",4::"int4",'input'::"text",'designer'::"text"))),

list_master AS (
   SELECT n.id from nodes n
   JOIN auth_user u ON n.user_id = u.id
   JOIN input ON n.typename = input.listTypeId
   WHERE u.username = input.masterUsername
   LIMIT 1
),

list_user AS(
    -- FIRST import parent from master to user list
    INSERT INTO nodes_ngrams (node_id,ngrams_id,parent_id,ngrams_type,list_type,weight)
    SELECT input.lid,nn.ngrams_id,nn.parent_id,nn.ngrams_type,nn.list_type, nn.weight
      FROM INPUT
      JOIN ngrams ng       ON ng.terms     = input.parent_terms
      JOIN nodes_ngrams nn ON nn.ngrams_id = ng.id
      JOIN list_master     ON nn.node_id   = list_master.id
    WHERE
      nn.ngrams_id = ng.id
      ON CONFLICT (node_id,ngrams_id,ngrams_type) DO NOTHING
    )

INSERT INTO nodes_ngrams (node_id,ngrams_id,parent_id,ngrams_type,list_type,weight)

SELECT input.lid, nc.id, nnpu.id, input.ntype, nnmaster.list_type, nnmaster.weight
FROM input

JOIN ngrams np ON np.terms = input.parent_terms
JOIN ngrams nc ON nc.terms = input.child_terms

JOIN nodes_ngrams nnpu     ON nnpu.ngrams_id     = np.id
JOIN nodes_ngrams nnmaster ON nnmaster.ngrams_id = nc.id
JOIN list_master           ON nnmaster.node_id   = list_master.id

WHERE
    nnpu.node_id     = input.lid
AND nnpu.ngrams_type = input.ntype

AND nnmaster.ngrams_id   = nc.id
AND nnmaster.ngrams_type = ntype

ON CONFLICT (node_id,ngrams_id,ngrams_type) DO
UPDATE SET parent_id = excluded.parent_id


  |]
  Del -> [sql|
WITH input(lid,listTypeId,masterUsername,ntype,parent_terms,child_terms)
  AS (?),
  -- (VALUES (15::"int4",5::"int4",'gargantua'::"text",4::"int4",'input'::"text",'designer'::"text"))),

list_master AS (
   SELECT n.id from nodes n
   JOIN auth_user u ON n.user_id = u.id
   JOIN input ON n.typename = input.listTypeId
   WHERE u.username = input.masterUsername
   LIMIT 1
),

list_user AS(
    -- FIRST import parent from master to user list
    INSERT INTO nodes_ngrams (node_id,ngrams_id,parent_id,ngrams_type,list_type,weight)
    SELECT input.lid,nn.ngrams_id,nn.parent_id,nn.ngrams_type,nn.list_type, nn.weight
      FROM INPUT
      JOIN ngrams ng       ON ng.terms     = input.parent_terms
      JOIN nodes_ngrams nn ON nn.ngrams_id = ng.id
      JOIN list_master     ON nn.node_id   = list_master.id
    WHERE
      nn.ngrams_id = ng.id
      ON CONFLICT (node_id,ngrams_id,ngrams_type) DO NOTHING
    )

INSERT INTO nodes_ngrams (node_id,ngrams_id,parent_id,ngrams_type,list_type,weight)

SELECT input.lid, nc.id, NULL, input.ntype, nnmaster.list_type, nnmaster.weight
FROM input

JOIN ngrams np ON np.terms = input.parent_terms
JOIN ngrams nc ON nc.terms = input.child_terms

JOIN nodes_ngrams nnmaster ON nnmaster.ngrams_id = nc.id
JOIN list_master           ON nnmaster.node_id   = list_master.id

WHERE
    nnmaster.ngrams_id   = nc.id
AND nnmaster.ngrams_type = ntype

ON CONFLICT (node_id,ngrams_id,ngrams_type) DO
UPDATE SET parent_id = NULL

  |]


data NodeNgramsUpdate = NodeNgramsUpdate
  { _nnu_user_list_id :: ListId
  , _nnu_lists_update :: [(NgramsTypeId, NgramsText, ListTypeId)]
  , _nnu_add_children :: [(NgramsTypeId, NgramsParent, NgramsChild)]
  , _nnu_rem_children :: [(NgramsTypeId, NgramsParent, NgramsChild)]
  }

-- TODO wrap these updates in a transaction.
-- TODO-ACCESS:
-- * check userId CanUpdateNgrams userListId
updateNodeNgrams :: NodeNgramsUpdate -> Cmd err ()
updateNodeNgrams nnu = do
  updateNodeNgrams' userListId $ _nnu_lists_update nnu
  ngramsGroup Del   userListId $ _nnu_rem_children nnu
  ngramsGroup Add   userListId $ _nnu_add_children nnu
  -- TODO remove duplicate line (fix SQL query)
  ngramsGroup Add   userListId $ _nnu_add_children nnu
  where
    userListId = _nnu_user_list_id nnu
