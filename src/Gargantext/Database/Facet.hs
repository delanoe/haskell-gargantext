{-|
Module      : Gargantext.Database.Facet
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}

module Gargantext.Database.Facet where

import Prelude hiding (null, id, map, sum, not)

import Gargantext.Types
import Gargantext.Types.Node (NodeType)
import Gargantext.Database.NodeNode
import Gargantext.Database.NodeNodeNgram
import Gargantext.Database.Node
import Gargantext.Database.Queries
import Gargantext.Utils.Prefix (unPrefix)
-- import Gargantext.Database.NodeNgram

-- import Data.Aeson (Value)
import Data.Aeson.TH (deriveJSON)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Opaleye.Internal.Join (NullMaker)

import qualified Opaleye.Internal.Unpackspec()
import Data.Profunctor.Product.Default (Default)

import Data.Time.Segment (jour)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)


-- DocFacet

type FacetDoc = Facet NodeId UTCTime HyperdataDocument Bool -- Double

data Facet id created hyperdata favorite  = FacetDoc { facetDoc_id :: id
                                                       , facetDoc_created   :: created
                                                       , facetDoc_hyperdata :: hyperdata
                                                       , facetDoc_favorite  :: favorite
                                          -- To be added: Double
                                          --             , facetDoc_ngramCount :: ngramCount
                                                       } deriving (Show)
$(deriveJSON (unPrefix "facetDoc_") ''Facet)

instance Arbitrary FacetDoc where
    arbitrary = elements [ FacetDoc id' (jour year 01 01) hp fav 
                         | id'  <- [1..10]
                         , year <- [1990..2000]
                         , fav  <- [True, False]
                         , hp   <- hyperdataDocuments
                         ]

-- Facets / Views for the Front End
type FacetDocRead  = Facet (Column PGInt4) (Column PGTimestamptz) (Column PGJsonb) (Column PGBool) -- (Column PGFloat8)

$(makeAdaptorAndInstance "pFacetDoc" ''Facet)
$(makeLensesWith abbreviatedFields   ''Facet)

getDocFacet :: Connection -> Int -> Maybe NodeType -> Maybe Offset -> Maybe Limit -> IO [FacetDoc]
getDocFacet conn parentId nodeType maybeOffset maybeLimit = 
    runQuery conn $ selectDocFacet parentId nodeType maybeOffset maybeLimit

selectDocFacet :: ParentId -> Maybe NodeType -> Maybe Offset -> Maybe Limit -> Query FacetDocRead
selectDocFacet parentId maybeNodeType maybeOffset maybeLimit =
        limit' maybeLimit $ offset' maybeOffset $ orderBy (asc facetDoc_created) $ selectDocFacet' parentId maybeNodeType


-- | Left join to the favorites
nodeNodeLeftJoin :: Query (NodeRead, NodeNodeReadNull)
nodeNodeLeftJoin = leftJoin queryNodeTable queryNodeNodeTable (eqNode)
    where
        eqNode (Node n1 _ _ _ _ _ _, NodeNode _ n2 _) = ((.==) n1 n2)


nodeNodeLeftJoin' :: (Column (Nullable PGInt4)) 
                  -> Query (NodeRead, NodeNodeReadNull)
nodeNodeLeftJoin' nId = leftJoin queryNodeTable queryNodeNodeTable (eqNode nId)
        where
            eqNode n (Node n1 _ _ _ _ _ _, NodeNode n1' n2 _) 
                   = foldl (.&&) (pgBool True) [ ((.==) n1 n2)
                                               , ((.==) n1' n)
                                               ]

nodeNodeLeftJoin'' :: Query (NodeRead, NodeRead, NodeNodeRead)
nodeNodeLeftJoin'' = join3 queryNodeTable queryNodeTable queryNodeNodeTable eqNode
        where
            eqNode (Node n1 _ _ _ _ _ _, Node n2 _ _ _ _ _ _, NodeNode n1' n2' _) 
                   = foldl (.&&) (pgBool True) [ ((.==) n2 n2')
                                               , ((.==) (toNullable n1) n1')
                                               ]

-- | Left join to the ngram count per document
nodeNodeNgramLeftJoin :: Query (NodeRead, NodeNodeNgramReadNull)
nodeNodeNgramLeftJoin = leftJoin queryNodeTable queryNodeNodeNgramTable (eqNode)
     where
        eqNode (Node n1 _ _ _ _ _ _, NodeNodeNgram n1' _ _ _) = ((.==) n1 n1')


nodeNodeNgramLeftJoin' :: Column (Nullable PGInt4) 
                       -> Query (NodeRead, NodeNodeNgramReadNull)
nodeNodeNgramLeftJoin' nId = leftJoin queryNodeTable queryNodeNodeNgramTable (eqNode nId)
     where
        eqNode nId' (Node n1 _ _ _ _ _ _, NodeNodeNgram n1' n2 _ _) 
                 = (.&&) ((.==) n1 n1')
                         ((.==) nId' (toNullable n2))



leftJoin3 :: (Default NullMaker (columnsL1, nullableColumnsR) nullableColumnsR1,
              Default NullMaker columnsR nullableColumnsR,
              Default Unpackspec columnsR columnsR,
              Default Unpackspec nullableColumnsR nullableColumnsR,
              Default Unpackspec columnsL1 columnsL1,
              Default Unpackspec columnsL columnsL) =>
              Query columnsL1 -> Query columnsR -> Query columnsL
                -> ((columnsL1, columnsR) -> Column PGBool)
                -> ((columnsL, (columnsL1, nullableColumnsR)) -> Column PGBool)
                -> Query (columnsL, nullableColumnsR1)
leftJoin3 q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q1 q2 cond12) cond23


leftJoin3' :: Query (NodeRead, (NodeReadNull, NodeNodeNgramReadNull))
leftJoin3' = leftJoin3 queryNodeTable  queryNodeNodeNgramTable queryNodeTable cond12 cond23
    where
         cond12 (Node favId _ _ _ _ _ _, NodeNodeNgram favId' _ _ _)
                = (.==) favId favId'

         cond23 :: (NodeRead, (NodeRead, NodeNodeNgramReadNull)) -> Column PGBool
         cond23 (Node  docId _ _ _ _ _ _, (Node _ _ _ _ _ _ _, NodeNodeNgram _ docId' _ _))
                = (.||) ((.==) (toNullable docId) docId') (isNull docId')


-- | Building the facet
selectDocFacet' :: ParentId -> Maybe NodeType -> Query FacetDocRead
selectDocFacet' parentId _ = proc () -> do
    node <- (proc () -> do

            -- Favorite Column
            (Node _ favTypeId _ favParentId _ _ _)      <- queryNodeTable -< ()
            restrict -< favTypeId .== 15 .&& favParentId .== (toNullable $ pgInt4 parentId)
            
            -- select nn.score from nodes n left join nodes_nodes nn on n.id = nn.node2_id where n.typename =4;
            -- Selecting the documents and joining Favorite Node
            (Node docId docTypeId _ docParentId _ created docHyperdata, NodeNode _ docTypeId' _) <- nodeNodeLeftJoin' (toNullable $ pgInt4 347537) -< ()
            restrict -< docParentId .== (toNullable $ pgInt4 parentId)
            let docTypeId'' = maybe 0 nodeTypeId (Just Document)
            restrict -< if docTypeId'' > 0
                           then docTypeId   .== (pgInt4 (docTypeId'' :: Int))
                           else (pgBool True)
            
            -- Getting favorite data
            let isFav = ifThenElse (isNull docTypeId') (pgBool False) (pgBool True)
            -- Ngram count by document
            -- Counting the ngram
            -- (Node occId occTypeId _ _ _ _ _, NodeNode _ _ _ count) <- nodeNodeNgramLeftJoin -< ()
            -- restrict -< occId .== 347540
            
            --returnA  -< (FacetDoc n_id hyperdata isFav ngramCount)) -< ()
            returnA  -< (FacetDoc docId created docHyperdata isFav)) -< ()
    returnA -< node


