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

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

------------------------------------------------------------------------
module Gargantext.Database.Facet 
  where
------------------------------------------------------------------------

import Prelude hiding (null, id, map, sum, not)
import GHC.Generics (Generic)

-- import Data.Aeson (Value)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Data.Time.Segment (jour)
import Data.Swagger

import           Database.PostgreSQL.Simple (Connection)
import           Opaleye
import           Opaleye.Internal.Join (NullMaker)
import qualified Opaleye.Internal.Unpackspec()

import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)

import Gargantext.Core.Types
import Gargantext.Core.Types.Node (NodeType)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.NodeNode
import Gargantext.Database.NodeNodeNgram
import Gargantext.Database.Node
import Gargantext.Database.Queries
-- import Gargantext.Database.NodeNgram

------------------------------------------------------------------------
-- | DocFacet

-- data Facet = FacetDoc | FacetSources | FacetAuthors | FacetTerms
--    deriving (Show, Generic)
--instance FromJSON Facet
--instance ToJSON   Facet

type FacetDoc = Facet NodeId UTCTime HyperdataDocument Bool Int
type FacetSources = FacetDoc
type FacetAuthors = FacetDoc
type FacetTerms   = FacetDoc



data Facet id created hyperdata favorite ngramCount = 
     FacetDoc { facetDoc_id         :: id
               , facetDoc_created    :: created
               , facetDoc_hyperdata  :: hyperdata
               , facetDoc_favorite   :: favorite
               , facetDoc_ngramCount :: ngramCount
               } deriving (Show, Generic)

-- | JSON instance

$(deriveJSON (unPrefix "facetDoc_") ''Facet)

-- | Documentation instance
instance ToSchema FacetDoc

-- | Mock and Quickcheck instances

instance Arbitrary FacetDoc where
    arbitrary = elements [ FacetDoc id' (jour year 01 01) hp fav ngramCount
                         | id'  <- [1..10]
                         , year <- [1990..2000]
                         , hp   <- hyperdataDocuments
                         , fav  <- [True, False]
                         , ngramCount <- [3..100]
                         ]

-- Facets / Views for the Front End
-- | Database instances
$(makeAdaptorAndInstance "pFacetDoc" ''Facet)
$(makeLensesWith abbreviatedFields   ''Facet)

type FacetDocRead = Facet (Column PGInt4       )
                          (Column PGTimestamptz)
                          (Column PGJsonb      )
                          (Column PGBool       )
                          (Column PGInt4       )

-----------------------------------------------------------------------

data FacetChart = FacetChart { facetChart_time  :: UTCTime'
                             , facetChart_count :: Double
                        }
        deriving (Show, Generic)
$(deriveJSON (unPrefix "facetChart_") ''FacetChart)
instance ToSchema FacetChart

instance Arbitrary FacetChart where
    arbitrary = FacetChart <$> arbitrary <*> arbitrary

-----------------------------------------------------------------------


getDocFacet :: Connection -> NodeType -> Int -> Maybe NodeType 
            -> Maybe Offset -> Maybe Limit
            -> IO [FacetDoc]
getDocFacet conn parentType parentId nodeType maybeOffset maybeLimit =
    runQuery conn $ selectDocFacet parentType parentId nodeType maybeOffset maybeLimit

selectDocFacet :: NodeType -> ParentId -> Maybe NodeType
               -> Maybe Offset -> Maybe Limit
               -> Query FacetDocRead
selectDocFacet pType parentId maybeNodeType maybeOffset maybeLimit =
        limit' maybeLimit $ offset' maybeOffset
                          $ orderBy (asc facetDoc_created)
                          $ selectDocFacet' pType parentId maybeNodeType


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
         cond12 (Node occId _ _ _ _ _ _, NodeNodeNgram occId' _ _ _)
                = (.==) occId occId'

         cond23 :: (NodeRead, (NodeRead, NodeNodeNgramReadNull)) -> Column PGBool
         cond23 (Node  docId _ _ _ _ _ _, (Node _ _ _ _ _ _ _, NodeNodeNgram _ docId' _ _))
                = (.||) ((.==) (toNullable docId) docId') (isNull docId')


leftJoin3''' :: Query (NodeRead, (NodeNodeReadNull, NodeReadNull))
leftJoin3''' = leftJoin3 queryNodeNodeTable queryNodeTable queryNodeTable cond12 cond23
    where
         cond12 (NodeNode favId _ _, Node favId' _ _ _ _ _ _)
                = (.||) ((.==) favId (toNullable favId')) (isNull $ toNullable favId)

         cond23 :: (NodeRead, (NodeNodeRead, NodeReadNull)) -> Column PGBool
         cond23 (Node  nId _ _ _ _ _ _, (NodeNode _ nId' _, Node _ _ _ _ _ _ _ ))
                = ((.==) (nId) (nId'))


-- | Building the facet
selectDocFacet' :: NodeType -> ParentId -> Maybe NodeType -> Query FacetDocRead
selectDocFacet' pt pId _ = proc () -> do
        (n1,(nn,n2)) <- leftJoin3''' -< ()
        restrict -< (.&&) (node_parentId n1 .== (toNullable $ pgInt4 pId))
                          (node_typename n1 .== (pgInt4 $ nodeTypeId Document))

        restrict -< (.||) (node_typename n2 .== (toNullable $ pgInt4 $ nodeTypeId Favorites))
                          (isNull $ node_typename n2)

        restrict -< (.||) (node_parentId n2 .== (toNullable $ pgInt4 $ nodeTypeId Favorites))
                          (isNull $ node_parentId n2)

        let isFav = ifThenElse (isNull $ nodeNode_score nn) (pgBool False) (pgBool True)

        returnA  -< FacetDoc (node_id n1) (node_date n1) (node_hyperdata n1) (isFav) (pgInt4 1)


