{-|
Module      : Gargantext.Database.Metrics.Count
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Count Ngrams by Context

-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Metrics.Count where

import Data.Monoid (mempty)
import Control.Arrow (returnA)
import Control.Lens (view)
import Data.Map.Strict (Map, fromListWith, elems)
import Data.Text (Text)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.API.Ngrams (NgramsElement(..))
import Gargantext.Core.Types.Main (listTypeId, ListType(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Queries.Join (leftJoin4, leftJoin5, leftJoin3)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Ngrams (NgramsId, NgramsType(..), ngramsTypeId, Ngrams(..), NgramsIndexed(..), ngrams, ngramsTerms, fromNgramsTypeId)
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Node (HasNodeError(..))
import Gargantext.Database.Schema.NodeNgram
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Prelude
import Gargantext.Text.Metrics.Count (Coocs, coocOn)
import Opaleye

getCoocByDocDev :: HasNodeError err => CorpusId -> ListId -> Cmd err (Map ([Text], [Text]) Int)
getCoocByDocDev cId lId = coocOn (\n-> [ view ( ngrams . ngramsTerms) n]) <$> getNgramsByDoc cId lId

getCoocByDoc :: CorpusId -> ListId -> Cmd err (Map (NgramsIndexed, NgramsIndexed) Coocs)
getCoocByDoc cId lId = coocOn identity <$> getNgramsByDoc cId lId


getNgramsByDoc :: CorpusId -> ListId -> Cmd err [[NgramsIndexed]]
getNgramsByDoc cId lId =
      elems
  <$> fromListWith (<>) 
  <$> map (\(nId, ngId, nt, n) -> (nId, [NgramsIndexed (Ngrams nt n) ngId]))
  <$> getNgramsByDocDb cId lId


getNgramsByDocDb :: CorpusId -> ListId -> Cmd err [(NodeId, NgramsId, Text, Int)]
getNgramsByDocDb cId lId = runPGSQuery query params
  where
    params = (cId, lId, listTypeId GraphList, ngramsTypeId NgramsTerms)
    query  = [sql|

    -- TODO add CTE
    SELECT n.id, ng.id, ng.terms, ng.n -- , list.parent_id
    FROM nodes n
    JOIN nodes_nodes  nn   ON nn.node2_id    = n.id
    JOIN nodes_ngrams nng  ON nng.node_id    = nn.node2_id
    JOIN nodes_ngrams list ON list.ngrams_id = nng.ngrams_id
    JOIN ngrams       ng   ON ng.id          = nng.ngrams_id
    WHERE nn.node1_id      = ? -- CorpusId
    AND   list.node_id     = ? -- ListId
    AND   list.list_type   = ? -- GraphListId
    AND   list.ngrams_type = ? -- NgramsTypeId

  |]


getNgramsByNode :: NodeId -> NgramsType -> Cmd err [[Text]]
getNgramsByNode nId nt =  elems
                   <$> fromListWith (<>)
                   <$> map (\(i,t) -> (i,[t]))
                   <$> getNgramsByNodeNodeIndexed nId nt

-- | TODO add join with nodeNodeNgram (if it exists)
getNgramsByNodeNodeIndexed :: NodeId -> NgramsType -> Cmd err [(NodeId, Text)]
getNgramsByNodeNodeIndexed nId nt = runOpaQuery (select' nId)
  where
    select' nId' = proc () -> do
      (ng,(nng,(nn,n))) <- getNgramsByNodeNodeIndexedJoin -< ()
      restrict          -< _node_id n         .== toNullable (pgNodeId nId')
      restrict          -< nng_ngramsType nng .== toNullable (pgNgramsTypeId $ ngramsTypeId nt)
      restrict          -< nn_delete      nn  ./= (toNullable . pgBool) True
      returnA           -< (nng_node_id nng, ngrams_terms ng)


{-
getNgramsByNodeIndexed' :: NodeId -> NgramsType -> Cmd err [(NodeId, Maybe Text)]
getNgramsByNodeIndexed' nId nt = runOpaQuery (select' nId)
  where
    select' nId' = proc () -> do
      (nnng,(ng,(nng,(_,n)))) <- getNgramsByNodeIndexedJoin5 -< ()
      restrict          -< _node_id n         .== toNullable (pgNodeId nId')
      restrict          -< nng_ngramsType nng .== toNullable (pgNgramsTypeId $ ngramsTypeId nt)

      let node_id' = ifThenElse (isNull $ toNullable $ nnng_node1_id nnng)
                          (nng_node_id nng)
                          (nnng_node2_id nng)
      let t1 = ifThenElse (isNull $ toNullable $ nnng_node1_id nnng)
                          (ngrams_terms ng)
                          (nnng_terms nng)
      returnA           -< (n1, t1)
--}

getNgramsByNodeNodeIndexedJoin :: Query ( NgramsRead
                                    , (NodeNgramReadNull
                                      , (NodeNodeReadNull
                                        , NodeReadNull
                                        )
                                      )
                                    )
getNgramsByNodeNodeIndexedJoin = leftJoin4 queryNodeTable
                                       queryNodeNodeTable
                                       queryNodeNgramTable
                                       queryNgramsTable
                                       c1 c2 c3
  where
    c1 :: (NodeNodeRead, NodeRead) -> Column PGBool
    c1 (nn,n)       = nn_node1_id nn .== _node_id n

    c2 :: ( NodeNgramRead
          , (NodeNodeRead
            , NodeReadNull
            )
          ) -> Column PGBool
    c2 (nng,(nn',_)) = (nng_node_id nng)   .== nn_node2_id nn'

    c3 :: ( NgramsRead
          , ( NodeNgramRead
            , ( NodeNodeReadNull
              , NodeReadNull
              )
            )
          ) -> Column PGBool
    c3 (ng,(nng',(_,_))) = (ngrams_id ng)   .== nng_ngrams_id nng'


getNgramsByNodeNodeIndexedJoin5 :: Query ( NodeNodeNgramsRead
                                    , (NgramsReadNull
                                      , (NodeNgramReadNull
                                        , (NodeNodeReadNull
                                          , NodeReadNull
                                          )
                                        )
                                      )
                                    )
getNgramsByNodeNodeIndexedJoin5 = leftJoin5 queryNodeTable
                                       queryNodeNodeTable
                                       queryNodeNgramTable
                                       queryNgramsTable
                                       queryNodeNodeNgramsTable
                                       c1 c2 c3 c4
  where
    c1 :: (NodeNodeRead, NodeRead) -> Column PGBool
    c1 (nn,n)       = nn_node1_id nn .== _node_id n

    c2 :: ( NodeNgramRead
          , (NodeNodeRead
            , NodeReadNull
            )
          ) -> Column PGBool
    c2 (nng,(nn',_)) = (nng_node_id nng)   .== nn_node2_id nn'

    c3 :: ( NgramsRead
          , ( NodeNgramRead
            , ( NodeNodeReadNull
              , NodeReadNull
              )
            )
          ) -> Column PGBool
    c3 (ng,(nng',(_,_))) = (ngrams_id ng)   .== nng_ngrams_id nng'

    c4 :: ( NodeNodeNgramsRead
            , (NgramsRead
              , ( NodeNgramReadNull
                , ( NodeNodeReadNull
                  , NodeReadNull
                  )
                )
              )
            ) -> Column PGBool
    c4 (nnng,(_,(_,(nn,_)))) =  (toNullable $ nnng_node1_id nnng) .== (nn_node1_id nn)
                            .&& (toNullable $ nnng_node2_id nnng) .== (nn_node2_id nn)

--}

--{-

getNgramsElementsWithParentNodeId :: NodeId -> Cmd err (Map NgramsType [NgramsElement])
getNgramsElementsWithParentNodeId nId = do
  ns <- getNgramsWithParentNodeId nId
  pure $ fromListWith (<>) [ (maybe (panic "error") identity $ fromNgramsTypeId nt, [NgramsElement ng CandidateList 1 Nothing mempty]) 
                | (_,(nt,ng)) <- ns
                ]


-------------------------------------------------------------------------
getNgramsWithParentNodeId :: NodeId -> Cmd err [(NodeId, (NgramsTypeId, Text))]
getNgramsWithParentNodeId nId = runOpaQuery (select nId)
  where
    select nId' = proc () -> do
      (ng,(nng,n)) <- getNgramsWithParentNodeIdJoin -< ()
      restrict -< _node_parentId n .== (toNullable $ pgNodeId nId')
      restrict -< _node_typename n .== (toNullable $ pgInt4 $ nodeTypeId NodeDocument)
      returnA  -< (nng_node_id nng, (nng_ngramsType nng, ngrams_terms ng))
--}

getNgramsWithParentNodeIdJoin :: Query ( NgramsRead
                                       , ( NodeNgramReadNull
                                         , NodeReadNull
                                         )
                                       )
getNgramsWithParentNodeIdJoin = leftJoin3 queryNodeTable queryNodeNgramTable queryNgramsTable on1 on2
  where
    on1 :: (NodeNgramRead, NodeRead) -> Column PGBool
    on1 (nng,n) = nng_node_id nng .== _node_id n

    on2 :: (NgramsRead, (NodeNgramRead, NodeReadNull))-> Column PGBool
    on2 (ng, (nng,_)) = ngrams_id ng .== nng_ngrams_id nng



