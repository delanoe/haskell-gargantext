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
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell           #-}
------------------------------------------------------------------------
module Gargantext.Database.Facet
  where
------------------------------------------------------------------------

import Prelude hiding (null, id, map, sum, not, read)
import Prelude (Enum, Bounded, minBound, maxBound)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Either(Either(Left))
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Segment (jour)
import Data.Swagger

import Database.PostgreSQL.Simple (Connection)
import           Opaleye
import qualified Opaleye.Internal.Unpackspec()

import Servant.API
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)

import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.NodeNode
import Gargantext.Database.Node
import Gargantext.Database.Queries
import Gargantext.Database.Config (nodeTypeId)
-- import Gargantext.Database.NodeNgram

------------------------------------------------------------------------
-- | DocFacet

-- data Facet = FacetDoc | FacetSources | FacetAuthors | FacetTerms
--    deriving (Show, Generic)
--instance FromJSON Facet
--instance ToJSON   Facet

type Favorite = Bool
type Title    = Text

type FacetDoc = Facet NodeId UTCTime Title HyperdataDocument Favorite Int
type FacetSources = FacetDoc
type FacetAuthors = FacetDoc
type FacetTerms   = FacetDoc



data Facet id created title hyperdata favorite ngramCount = 
     FacetDoc { facetDoc_id         :: id
              , facetDoc_created    :: created
              , facetDoc_title      :: title
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
    arbitrary = elements [ FacetDoc id' (jour year 01 01) t hp fav ngramCount
                         | id'  <- [1..10]
                         , year <- [1990..2000]
                         , t    <- ["title", "another title"]
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
                          (Column PGText       )
                          (Column PGJsonb      )
                          (Column PGBool)
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
type Trash   = Bool
data OrderBy =  DateAsc | DateDesc
             | TitleAsc | TitleDesc
             | FavDesc  | FavAsc
             deriving (Generic, Enum, Bounded, Read, Show)
             -- | NgramCoun

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "DateAsc"  = pure DateAsc
    parseUrlPiece "DateDesc" = pure DateDesc
    parseUrlPiece "TitleAsc" = pure TitleAsc
    parseUrlPiece "TitleDesc" = pure TitleDesc
    parseUrlPiece "FavAsc"   = pure FavAsc
    parseUrlPiece "FavDesc"   = pure FavDesc
    parseUrlPiece _           = Left "Unexpected value of OrderBy"

instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]

viewDocuments :: CorpusId -> Trash -> NodeTypeId -> Query FacetDocRead
viewDocuments cId t ntId = proc () -> do
  n  <- queryNodeTable -< ()
  nn <- queryNodeNodeTable -< ()
  restrict -< _node_id n .== nodeNode_node2_id nn
  restrict -< nodeNode_node1_id nn .== (pgInt4 cId)
  restrict -< _node_typename    n  .== (pgInt4 ntId)
  restrict -< nodeNode_delete   nn .== (pgBool t)
  returnA  -< FacetDoc (_node_id n) (_node_date n) (_node_name n) (_node_hyperdata n) (nodeNode_favorite nn) (pgInt4 1)


filterDocuments :: (PGOrd date, PGOrd title, PGOrd favorite) =>
     Maybe Gargantext.Core.Types.Offset
     -> Maybe Gargantext.Core.Types.Limit
     -> Maybe OrderBy
     -> Select (Facet id (Column date) (Column title) hyperdata (Column favorite) ngramCount)
     -> Query  (Facet id (Column date) (Column title) hyperdata (Column favorite) ngramCount)
filterDocuments o l order q = limit' l $ offset' o $ orderBy ordering q
  where
    ordering = case order of
      (Just DateAsc)   -> asc  facetDoc_created
      
      (Just TitleAsc)  -> asc  facetDoc_title
      (Just TitleDesc) -> desc facetDoc_title
      
      (Just FavAsc)    -> asc  facetDoc_favorite
      (Just FavDesc)   -> desc facetDoc_favorite
      _                -> desc facetDoc_created


runViewDocuments :: CorpusId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd [FacetDoc]
runViewDocuments cId t o l order = mkCmd $ \c -> runViewDocuments' c cId t o l order

-- | TODO use only Cmd with Reader and delete function below
runViewDocuments' :: Connection -> CorpusId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> IO [FacetDoc]
runViewDocuments' c cId t o l order = runQuery c ( filterDocuments o l order
                                                $ viewDocuments cId t ntId)
  where
    ntId = nodeTypeId NodeDocument




{-
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
        eqNode (Node n1 _ _ _ _ _ _, NodeNode _ n2 _ _ _ ) = ((.==) n1 n2)


nodeNodeLeftJoin' :: (Column (Nullable PGInt4)) 
                  -> Query (NodeRead, NodeNodeReadNull)
nodeNodeLeftJoin' nId = leftJoin queryNodeTable queryNodeNodeTable (eqNode nId)
        where
            eqNode n (Node n1 _ _ _ _ _ _, NodeNode n1' n2 _ _ _)
                   = foldl (.&&) (pgBool True) [ ((.==) n1 n2)
                                               , ((.==) n1' n)
                                               ]

nodeNodeLeftJoin'' :: Query (NodeRead, NodeRead, NodeNodeRead)
nodeNodeLeftJoin'' = join3 queryNodeTable queryNodeTable queryNodeNodeTable eqNode
        where
            eqNode (Node n1 _ _ _ _ _ _, Node n2 _ _ _ _ _ _, NodeNode n1' n2' _ _ _)
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
         cond12 (NodeNode favId _ _ _ _, Node favId' _ _ _ _ _ _)
                = (.||) ((.==) favId (toNullable favId')) (isNull $ toNullable favId)

         cond23 :: (NodeRead, (NodeNodeRead, NodeReadNull)) -> Column PGBool
         cond23 (Node  nId _ _ _ _ _ _, (NodeNode _ nId' _ _ _, Node _ _ _ _ _ _ _ ))
                = ((.==) (nId) (nId'))


-- | Building the facet
selectDocFacet' :: NodeType -> ParentId -> Maybe NodeType -> Query FacetDocRead
selectDocFacet' _ pId _ = proc () -> do
        (n1,(nn,_n2)) <- leftJoin3''' -< ()
        restrict -< (.&&) (_node_parentId n1 .== (toNullable $ pgInt4 pId))
                          (_node_typename n1 .== (pgInt4 $ nodeTypeId NodeDocument))

--        restrict -< (.||) (node_typename n2 .== (toNullable $ pgInt4 $ nodeTypeId Favorites))
--                          (isNull $ node_typename n2)
--
--        restrict -< (.||) (node_parentId n2 .== (toNullable $ pgInt4 $ nodeTypeId Favorites))
--                          (isNull $ node_parentId n2)

        let isFav = ifThenElse (isNull $ nodeNode_score nn) (pgBool False) (pgBool True)

        returnA  -< FacetDoc (_node_id n1) (_node_date n1) (_node_hyperdata n1) (isFav) (pgInt4 1)

-}
